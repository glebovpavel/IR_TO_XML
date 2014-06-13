CREATE OR REPLACE PACKAGE IR_TO_XML AS    
  
  -- download interactive report as PDF
  PROCEDURE get_report_xml(p_app_id          IN NUMBER,
                           p_page_id         in number,                                
                           p_return_type     IN CHAR DEFAULT 'X', -- "Q" for debug information "X" for XML-Data
                           p_get_page_items  IN CHAR DEFAULT 'N', -- Y,N - include page items in XML
                           p_items_list      in varchar2,         -- "," delimetered list of items that for including in XML
                           p_collection_name IN VARCHAR2,         -- name of APEX COLLECTION to save XML, when null - download as file
                           p_max_rows        IN NUMBER            -- maximum rows for export                            
                          );
  --return debug information
  function get_log return clob;
                              
END IR_TO_XML;
/


CREATE OR REPLACE PACKAGE BODY IR_TO_XML AS   
 
  cursor cur_highlight(p_report_id in APEX_APPLICATION_PAGE_IR_RPT.REPORT_ID%TYPE,
                       p_delimetered_column_list in varchar2) 
  IS
  select replace(replace(replace(replace(condition_sql,'#APXWS_EXPR#',''''||CONDITION_EXPRESSION||''''),'#APXWS_EXPR2#',''''||CONDITION_EXPRESSION2||''''),'#APXWS_HL_ID#','1'),'#APXWS_CC_EXPR#','"'||CONDITION_COLUMN_NAME||'"')  condition_sql,
       CONDITION_COLUMN_NAME,
       CONDITION_ENABLED,
       HIGHLIGHT_ROW_COLOR,
       HIGHLIGHT_ROW_FONT_COLOR,
       HIGHLIGHT_CELL_COLOR,
       HIGHLIGHT_CELL_FONT_COLOR,
       rownum COND_NUMBER,
       'HIGHLIGHT_'||rownum COND_NAME
  from APEX_APPLICATION_PAGE_IR_COND
  where condition_type = 'Highlight'
    and report_id = p_report_id
    and condition_enabled = 'Yes'
    and instr(':'||p_delimetered_column_list||':',':'||CONDITION_COLUMN_NAME||':') > 0
    order by --rows highlights first 
           nvl2(HIGHLIGHT_ROW_COLOR,1,0) desc, 
           nvl2(HIGHLIGHT_ROW_FONT_COLOR,1,0) desc,
           HIGHLIGHT_SEQUENCE;
  
  type t_col_names is table of apex_application_page_ir_col.report_label%type index by apex_application_page_ir_col.column_alias%type;
  type t_col_format_mask is table of APEX_APPLICATION_PAGE_IR_COMP.computation_format_mask%TYPE index by APEX_APPLICATION_PAGE_IR_COL.column_alias%TYPE;
  type t_header_alignment is table of APEX_APPLICATION_PAGE_IR_COL.heading_alignment%TYPE index by APEX_APPLICATION_PAGE_IR_COL.column_alias%TYPE;
  type t_column_alignment is table of apex_application_page_ir_col.column_alignment%type index by apex_application_page_ir_col.column_alias%type;
  type t_column_types is table of apex_application_page_ir_col.column_type%type index by apex_application_page_ir_col.column_alias%type;
  type t_highlight is table of cur_highlight%ROWTYPE index by binary_integer;
  
  type ir_report is record
   (
    report                    apex_ir.t_report,
    ir_data                   APEX_APPLICATION_PAGE_IR_RPT%ROWTYPE,
    displayed_columns         APEX_APPLICATION_GLOBAL.VC_ARR2,
    break_on                  APEX_APPLICATION_GLOBAL.VC_ARR2,
    break_really_on           APEX_APPLICATION_GLOBAL.VC_ARR2, -- "break on" except hidden columns
    sum_columns_on_break      APEX_APPLICATION_GLOBAL.VC_ARR2,
    avg_columns_on_break      APEX_APPLICATION_GLOBAL.VC_ARR2,
    max_columns_on_break      APEX_APPLICATION_GLOBAL.VC_ARR2,
    min_columns_on_break      APEX_APPLICATION_GLOBAL.VC_ARR2,
    median_columns_on_break   APEX_APPLICATION_GLOBAL.VC_ARR2,
    count_columns_on_break    APEX_APPLICATION_GLOBAL.VC_ARR2,
    count_distnt_col_on_break APEX_APPLICATION_GLOBAL.VC_ARR2,
    skipped_columns           INTEGER default 0, -- when scpecial coluns like apxws_row_pk is used
    start_with                INTEGER default 0, -- position of first displayed column in query
    end_with                  INTEGER default 0, -- position of last displayed column in query
    agg_cols_cnt              INTEGER default 0, 
    column_names              t_col_names,       -- column names in report header
    col_format_mask           t_col_format_mask, -- format like $3849,56
    row_highlight             t_highlight,
    col_highlight             t_highlight,
    header_alignment          t_header_alignment,
    column_alignment          t_column_alignment,
    column_types              t_column_types  
   );  

   TYPE t_data IS record
   (
     VALUE           VARCHAR2(100),
     text            CLOB
   );  

  l_report    ir_report;   
  v_debug     clob;
  ------------------------------------------------------------------------------
  function get_log
  return clob
  is
  begin
    return v_debug;
  end  get_log;
  ------------------------------------------------------------------------------
  procedure log(p_message in varchar2)
  is
  begin
    v_debug := v_debug||p_message||chr(10);
    apex_debug_message.log_message(p_message => substr(p_message,1,32767),
                                   p_enabled => false,
                                   p_level   => 4);    
  end log; 
  ------------------------------------------------------------------------------
  function bcoll(p_font_color    in varchar2 default null,
                 p_back_color    in varchar2 default null,
                 p_align         in varchar2 default null,
                 p_width         in varchar2 default null,
                 p_column_alias  IN VARCHAR2 DEFAULT NULL,
                 p_colmn_type    IN VARCHAR2 DEFAULT NULL,
                 p_value         IN VARCHAR2 DEFAULT NULL,
                 p_format_mask   IN VARCHAR2 DEFAULT NULL) 
  return varchar2
  is
    v_str varchar2(500);
  begin
    v_str := v_str||'<CELL ';
    if p_column_alias is not null then v_str := v_str||'column-alias="'||p_column_alias||'" '; end if;
    if p_font_color is not null then v_str := v_str||'color="'||p_font_color||'" '; end if;
    if p_colmn_type is not null then V_STR := V_STR||'data-type="'||p_colmn_type||'" '; end if;
    if p_back_color is not null then v_str := v_str||'background-color="'||p_back_color||'" '; end if;
    if p_align is not null then V_STR := V_STR||'align="'||lower(p_align)||'" '; end if;
    IF p_width IS NOT NULL THEN v_str := v_str||'width="'||p_width||'" '; END IF;        
    IF p_value IS NOT NULL THEN v_str := v_str||'value="'||p_value||'" '; END IF;
    if p_format_mask is not null then v_str := v_str||'format_mask="'||p_format_mask||'" '; end if;
    v_str := v_str||'>'; 
    
    return v_str;
  end bcoll;
  ------------------------------------------------------------------------------
  function ecoll(i integer) 
  return varchar2
  is
  begin
   return '</CELL>';
  end ecoll;
    ------------------------------------------------------------------------------
  function get_column_names(p_column_alias in apex_application_page_ir_col.column_alias%type)
  return APEX_APPLICATION_PAGE_IR_COL.report_label%TYPE
  is
  begin
    return l_report.column_names(p_column_alias);
  exception
    when others then
       raise_application_error(-20001,'get_column_names: p_column_alias='||p_column_alias||' '||SQLERRM);
  end get_column_names;
  ------------------------------------------------------------------------------
  function get_col_format_mask(p_column_alias in apex_application_page_ir_col.column_alias%type)
  return APEX_APPLICATION_PAGE_IR_COMP.computation_format_mask%TYPE
  is
  begin
    return l_report.col_format_mask(p_column_alias);
  exception
    when others then
       raise_application_error(-20001,'get_column_names: p_column_alias='||p_column_alias||' '||SQLERRM);
  end get_col_format_mask;
  ------------------------------------------------------------------------------
  function get_header_alignment(p_column_alias in apex_application_page_ir_col.column_alias%type)
  return APEX_APPLICATION_PAGE_IR_COL.heading_alignment%TYPE
  is
  begin
    return l_report.header_alignment(p_column_alias);
  exception
    when others then
       raise_application_error(-20001,'get_column_names: p_column_alias='||p_column_alias||' '||SQLERRM);
  end get_header_alignment;
  ------------------------------------------------------------------------------
  function get_column_alignment(p_column_alias in apex_application_page_ir_col.column_alias%type)
  return apex_application_page_ir_col.column_alignment%type
  is
  begin
    return l_report.column_alignment(p_column_alias);
  exception
    when others then
       raise_application_error(-20001,'get_column_names: p_column_alias='||p_column_alias||' '||SQLERRM);
  end get_column_alignment;
  ------------------------------------------------------------------------------
  function get_column_types(p_column_alias in apex_application_page_ir_col.column_alias%type)
  return apex_application_page_ir_col.column_type%type
  is
  begin
    return l_report.column_types(p_column_alias);
  exception
    when others then
       raise_application_error(-20001,'get_column_names: p_column_alias='||p_column_alias||' '||SQLERRM);
  END get_column_types;
  ------------------------------------------------------------------------------  
  function get_column_alias(p_num in binary_integer)
  return varchar2
  is
  begin
    return l_report.displayed_columns(p_num);
  exception
    when others then
       raise_application_error(-20001,'get_column_alias: p_num='||p_num||' '||SQLERRM);
  END get_column_alias;
  ------------------------------------------------------------------------------
  FUNCTION get_column_alias_sql(p_num IN binary_integer -- column number in sql-query
                               )
  return varchar2
  is
  BEGIN
    return l_report.displayed_columns(p_num - l_report.start_with + 1);
  exception
    WHEN others THEN
       raise_application_error(-20001,'get_column_alias_sql: p_num='||p_num||' '||SQLERRM);
  END get_column_alias_sql;
  ------------------------------------------------------------------------------
  function get_current_row(p_current_row in apex_application_global.vc_arr2,
                           p_id in binary_integer)
  return apex_application_page_ir_col.column_type%type
  is
  begin
    return p_current_row(p_id);
  exception
    when others then
       raise_application_error(-20001,'get_current_row: p_id='||p_id||' '||SQLERRM);
  end get_current_row; 
  ------------------------------------------------------------------------------
  -- :::: -> :
  function rr(p_str in varchar2)
  return varchar2
  is 
  begin
    return ltrim(rtrim(regexp_replace(p_str,'[:]+',':'),':'),':');
  end;
  ------------------------------------------------------------------------------   
  FUNCTION get_xmlval(p_str IN CLOB)
  return clob
  is   
  BEGIN
    RETURN REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(p_str,'<','%26lt;'),'>','%26gt;'),'&','%26amp;'),'"','%26quot;'),'''','%26apos;');
  end get_xmlval;  
  ------------------------------------------------------------------------------  
  function intersect_arrays(p_one IN APEX_APPLICATION_GLOBAL.VC_ARR2,
                            p_two IN APEX_APPLICATION_GLOBAL.VC_ARR2)
  return APEX_APPLICATION_GLOBAL.VC_ARR2
  is    
    v_ret APEX_APPLICATION_GLOBAL.VC_ARR2;
  begin    
    for i in 1..p_one.count loop
       for b in 1..p_two.count loop
         if p_one(i) = p_two(b) then
            v_ret(v_ret.count + 1) := p_one(i);
           exit;
         end if;
       end loop;        
    end loop;
    
    return v_ret;
  end intersect_arrays;
  ------------------------------------------------------------------------------
  function intersect_count(p_one IN APEX_APPLICATION_GLOBAL.VC_ARR2,
                           p_two IN APEX_APPLICATION_GLOBAL.VC_ARR2)
  return integer
  is
   v_rez APEX_APPLICATION_GLOBAL.VC_ARR2;
  begin
    v_rez := intersect_arrays(p_one,p_two);
    return v_rez.count;
  end intersect_count; 
  ------------------------------------------------------------------------------
  
  procedure init_t_report(p_app_id       in number,
                          p_page_id      in number)
  is
    l_region_id     number;
    l_report_id     number;
    v_query_targets APEX_APPLICATION_GLOBAL.VC_ARR2;
  begin
    select region_id 
    into l_region_id 
    from APEX_APPLICATION_PAGE_REGIONS 
    where application_id = p_app_id 
      and page_id = p_page_id 
      and source_type = 'Interactive Report';    
    
    --get base report id    
    log('l_region_id='||l_region_id);
    
    l_report_id := apex_ir.get_last_viewed_report_id (p_page_id   => p_page_id,
                                                      p_region_id => l_region_id);
    
    log('l_base_report_id='||l_report_id);
    
    select r.* 
    into l_report.ir_data       
    from apex_application_page_ir_rpt r
    where application_id = p_app_id 
      and page_id = p_page_id
      and session_id = v('APP_SESSION')
      and application_user = v('APP_USER')
      and base_report_id = l_report_id;
  
    log('l_report_id='||l_report_id);
    l_report_id := l_report.ir_data.report_id;                                                                 
      
      
    l_report.report := apex_ir.get_report (p_page_id        => p_page_id,
                                           p_region_id      => l_region_id
                                           --p_report_id      => l_report_id
                                          );
    for i in (select column_alias,
                     report_label,
                     heading_alignment,
                     column_alignment,
                     column_type,
                     format_mask AS  computation_format_mask,
                     instr(l_report.ir_data.report_columns,column_alias) column_order ,
                     instr(l_report.ir_data.break_enabled_on,column_alias) break_column_order
                from APEX_APPLICATION_PAGE_IR_COL
               where application_id = p_app_id
                 AND page_id = p_page_id
                 AND display_text_as != 'HIDDEN' --after report RESETTING l_report.ir_data.report_columns consists HIDDEN column - APEX bug????
                 and instr(l_report.ir_data.report_columns,column_alias) > 0
              UNION
              select computation_column_alias,
                     computation_report_label,
                     'center' as heading_alignment,
                     'right' AS column_alignment,
                     computation_column_type,
                     computation_format_mask,
                     instr(l_report.ir_data.report_columns,computation_column_alias) column_order,
                     instr(l_report.ir_data.break_enabled_on,computation_column_alias) break_column_order
              from APEX_APPLICATION_PAGE_IR_COMP
              where application_id = p_app_id
                and page_id = p_page_id
                AND report_id = l_report_id
                AND instr(l_report.ir_data.report_columns,computation_column_alias) > 0
              order by  break_column_order asc,column_order asc)
    loop                 
      l_report.column_names(i.column_alias) := i.report_label; 
      l_report.col_format_mask(i.column_alias) := i.computation_format_mask;
      l_report.header_alignment(i.column_alias) := i.heading_alignment; 
      l_report.column_alignment(i.column_alias) := i.column_alignment; 
      l_report.column_types(i.column_alias) := i.column_type;
      IF i.column_order > 0 THEN
        IF i.break_column_order = 0 THEN 
          --displayed column
          l_report.displayed_columns(l_report.displayed_columns.count + 1) := i.column_alias;
        ELSE  
          --break column
          l_report.break_really_on(l_report.break_really_on.count + 1) := i.column_alias;
        end if;
      end if;  
      
      log('column='||i.column_alias||' l_report.column_names='||i.report_label);
      log('column='||i.column_alias||' l_report.col_format_mask='||i.computation_format_mask);
      log('column='||i.column_alias||' l_report.header_alignment='||i.heading_alignment);
      log('column='||i.column_alias||' l_report.column_alignment='||i.column_alignment);
      log('column='||i.column_alias||' l_report.column_types='||i.column_type);
    end loop;    

    --l_report.break_on := APEX_UTIL.STRING_TO_TABLE(rr(l_report.ir_data.break_enabled_on));    
    l_report.sum_columns_on_break := APEX_UTIL.STRING_TO_TABLE(rr(l_report.ir_data.sum_columns_on_break));  
    l_report.avg_columns_on_break := APEX_UTIL.STRING_TO_TABLE(rr(l_report.ir_data.avg_columns_on_break));  
    l_report.max_columns_on_break := APEX_UTIL.STRING_TO_TABLE(rr(l_report.ir_data.max_columns_on_break));  
    l_report.min_columns_on_break := APEX_UTIL.STRING_TO_TABLE(rr(l_report.ir_data.min_columns_on_break));  
    l_report.median_columns_on_break := APEX_UTIL.STRING_TO_TABLE(rr(l_report.ir_data.median_columns_on_break)); 
    l_report.count_columns_on_break := APEX_UTIL.STRING_TO_TABLE(rr(l_report.ir_data.count_columns_on_break));  
    l_report.count_distnt_col_on_break := APEX_UTIL.STRING_TO_TABLE(rr(l_report.ir_data.count_distnt_col_on_break)); 
      
    l_report.agg_cols_cnt := l_report.sum_columns_on_break.count + 
                             l_report.avg_columns_on_break.count +
                             l_report.max_columns_on_break.count + 
                             l_report.min_columns_on_break.count +
                             l_report.median_columns_on_break.count +
                             l_report.count_columns_on_break.count +
                             l_report.count_distnt_col_on_break.count;
    
    log('l_report.displayed_columns='||rr(l_report.ir_data.report_columns));
    log('l_report.break_on='||rr(l_report.ir_data.break_enabled_on));
    log('l_report.sum_columns_on_break='||rr(l_report.ir_data.sum_columns_on_break));
    log('l_report.avg_columns_on_break='||rr(l_report.ir_data.avg_columns_on_break));
    log('l_report.max_columns_on_break='||rr(l_report.ir_data.max_columns_on_break));
    LOG('l_report.min_columns_on_break='||rr(l_report.ir_data.min_columns_on_break));
    log('l_report.median_columns_on_break='||rr(l_report.ir_data.median_columns_on_break));
    log('l_report.count_columns_on_break='||rr(l_report.ir_data.count_distnt_col_on_break));
    log('l_report.count_distnt_col_on_break='||rr(l_report.ir_data.count_columns_on_break));
    log('l_report.break_really_on='||APEX_UTIL.TABLE_TO_STRING(l_report.break_really_on));
    log('l_report.agg_cols_cnt='||l_report.agg_cols_cnt);
    
    v_query_targets(v_query_targets.count + 1) := 'rez.*';
     
    for c in cur_highlight(p_report_id => l_report_id,
                           p_delimetered_column_list => l_report.ir_data.report_columns
                          ) 
    loop
        if c.HIGHLIGHT_ROW_COLOR is not null or c.HIGHLIGHT_ROW_FONT_COLOR is not null then
          --is row highlight
          l_report.row_highlight(l_report.row_highlight.count + 1) := c;        
        else
          l_report.col_highlight(l_report.col_highlight.count + 1) := c;           
        end if;  
        v_query_targets(v_query_targets.count + 1) := c.condition_sql;
    end loop;    
        
    l_report.report.sql_query := 'SELECT '||APEX_UTIL.TABLE_TO_STRING(v_query_targets,',')||' from ( '
                                          ||l_report.report.sql_query||' ) rez';
    log('l_report.report.sql_query='||chr(10)||l_report.report.sql_query||chr(10));
  exception
    when no_data_found then
      raise_application_error(-20001,'No Interactive Report found on Page='||p_page_id||' Application='||p_app_id||' Please make sure that the report was running at least once by this session.');
    when others then 
      raise_application_error(-20001,'get_t_report: Page='||p_page_id||' Application='||p_app_id||' '||sqlerrm);
  end init_t_report;  
  ------------------------------------------------------------------------------
 
  function is_control_break(p_curr_row  IN APEX_APPLICATION_GLOBAL.VC_ARR2,
                            p_prev_row  IN APEX_APPLICATION_GLOBAL.VC_ARR2)
  return boolean
  is
    v_start_with      integer;
    v_end_with        integer;    
    v_tmp             integer;
  begin
    if nvl(l_report.break_really_on.count,0) = 0  then
      return false; --no control break
    end if;
    v_start_with := 1 + l_report.skipped_columns;    
    v_end_with   := l_report.skipped_columns + nvl(l_report.break_really_on.count,0);
    for i in v_start_with..v_end_with loop
      if p_curr_row(i) != p_prev_row(i) then
        return true;
      end if;
    end loop;
    return false;
  end is_control_break;
  ------------------------------------------------------------------------------
  FUNCTION get_date(p_query_value in varchar2,p_format_mask in varchar2)
  RETURN t_data
  IS
    v_data t_data;
  BEGIN
     BEGIN
       v_data.value := to_date(p_query_value) - to_date('01-03-1900','DD-MM-YYYY') + 61;
       if p_format_mask is not null then
         v_data.text := to_char(to_date(p_query_value),p_format_mask);
       ELSE
         v_data.text := p_query_value;
       end if;
      exception
        WHEN others THEN 
          v_data.text := p_query_value;
      END;      
  end get_date;
  ------------------------------------------------------------------------------  
  function print_row(p_current_row     IN APEX_APPLICATION_GLOBAL.VC_ARR2)
  RETURN CLOB IS
    v_clob            clob;
    v_column_alias    APEX_APPLICATION_PAGE_IR_COL.column_alias%TYPE;
    v_format_mask     APEX_APPLICATION_PAGE_IR_COMP.computation_format_mask%TYPE;
    v_row_color       varchar2(10); 
    v_row_back_color  varchar2(10);
    v_cell_color      varchar2(10);
    v_cell_back_color VARCHAR2(10);     
    v_src             VARCHAR2(100);
    v_value           VARCHAR2(100);
    v_text            clob;
    v_column_type     VARCHAR2(10);
  begin
    --check that row need to be highlighted
    <<row_highlights>>
    for h in 1..l_report.row_highlight.count loop
     BEGIN 
      IF get_current_row(p_current_row,l_report.end_with + l_report.agg_cols_cnt + l_report.row_highlight(h).COND_NUMBER) IS NOT NULL THEN
         v_row_color       := l_report.row_highlight(h).HIGHLIGHT_ROW_FONT_COLOR;
         v_row_back_color  := l_report.row_highlight(h).HIGHLIGHT_ROW_COLOR;
      END IF;
     exception       
       when no_data_found then
         log('row_highlights: ='||' end_with='||l_report.end_with||' agg_cols_cnt='||l_report.agg_cols_cnt||' COND_NUMBER='||l_report.row_highlight(h).cond_number||' h='||h);
     end; 
    end loop row_highlights;
    --
    <<visible_columns>>
    for i in l_report.start_with..l_report.end_with loop
      v_cell_color       := null;
      v_cell_back_color  := null;
    
      v_column_alias := get_column_alias_sql(i);
      v_column_type := get_column_types(v_column_alias);
      v_format_mask := get_col_format_mask(v_column_alias);
      IF v_column_type = 'DATE' THEN
         v_src := get_current_row(p_current_row,i);
         BEGIN
           v_value := to_date(v_src) - to_date('01-03-1900','DD-MM-YYYY') + 61;
           if v_format_mask is not null then
             v_text := to_char(to_date(v_src),v_format_mask);
           ELSE
             v_text := v_src;
           end if;
          exception
            WHEN others THEN 
              v_text := v_src;
          END;
          v_format_mask := regexp_replace(get_col_format_mask(v_column_alias),'[^a-zA-Z0-9+-.,/\ :]','');          
      ELSIF  v_column_type = 'NUMBER' THEN      
        v_src := get_current_row(p_current_row,i);
        BEGIN
          v_value := v_src;
          IF v_format_mask IS NOT NULL THEN
            v_text := to_char(to_number(v_src),v_format_mask);
          ELSE
            v_text := v_src;
          end if;
        exception
            WHEN others THEN 
              v_text := v_src;
        end;
      ELSE --STRING
        v_format_mask := NULL;
        v_value := NULL;
        v_text := get_current_row(p_current_row,i);
      end if; 
       
      --check that cell need to be highlighted
      <<column_highlights>>
      for h in 1..l_report.col_highlight.count loop
        begin
          --debug
          if get_current_row(p_current_row,l_report.end_with + l_report.agg_cols_cnt + l_report.col_highlight(h).COND_NUMBER) is not null 
             and v_column_alias = l_report.col_highlight(h).CONDITION_COLUMN_NAME 
          then
            v_cell_color       := l_report.col_highlight(h).HIGHLIGHT_CELL_FONT_COLOR;
            v_cell_back_color  := l_report.col_highlight(h).HIGHLIGHT_CELL_COLOR;
          end if;
        exception
       when no_data_found then
         log('col_highlights: ='||' end_with='||l_report.end_with||' agg_cols_cnt='||l_report.agg_cols_cnt||' COND_NUMBER='||l_report.col_highlight(h).cond_number||' h='||h); 
       end;
      end loop column_highlights;
      v_clob := v_clob ||bcoll(p_font_color   => nvl(v_cell_color,v_row_color),
                               p_back_color   => nvl(v_cell_back_color,v_row_back_color),
                               p_align        => get_column_alignment(v_column_alias),
                               p_column_alias => v_column_alias,
                               p_colmn_type   => v_column_type,
                               p_value        => v_value,
                               p_format_mask  => regexp_replace(v_format_mask,'[^a-zA-Z0-9+-.,/\ :]','')
                              )
                       ||get_xmlval(v_text)
                       ||ecoll(i);
    end loop visible_columns;
    
    return  '<ROW>'||v_clob || '</ROW>'||chr(10);    
  end print_row;
  
  ------------------------------------------------------------------------------ 
 
  function print_header
  return clob is
    v_clob            clob;
    v_column_alias    APEX_APPLICATION_PAGE_IR_COL.column_alias%TYPE;
  begin
    v_clob := v_clob || '<HEADER>';
    <<headers>>
    for i in 1..l_report.displayed_columns.count   loop
      V_COLUMN_ALIAS := get_column_alias(i);
      -- if current column is not control break column
      if apex_plugin_util.get_position_in_list(l_report.break_on,v_column_alias) is null then      
        v_clob := v_clob ||bcoll(p_column_alias=>v_column_alias,p_align=>get_header_alignment(v_column_alias))
                         ||get_xmlval(get_column_names(v_column_alias))
                         ||ecoll(i);
      end if;  
    end loop headers;
    return  v_clob || '</HEADER>'||chr(10);
  end print_header; 
  ------------------------------------------------------------------------------  
  function print_control_break_header(p_current_row     IN APEX_APPLICATION_GLOBAL.VC_ARR2) 
  return clob
  is
    v_clob            clob;
  begin
    if nvl(l_report.break_really_on.count,0) = 0  then
      return ''; --no control break
    end if;
    
    <<break_columns>>
    for i in 1..nvl(l_report.break_really_on.count,0) loop
      --TODO: Add column header
      v_clob := v_clob ||get_column_names(l_report.break_really_on(i))||': '||get_current_row(p_current_row,i + l_report.skipped_columns)||',';
    end loop visible_columns;
    
    return  '<BREAK_HEADER>'||get_xmlval(rtrim(v_clob,',')) || '</BREAK_HEADER>'||chr(10);    
  end print_control_break_header;
  ------------------------------------------------------------------------------
  function find_rel_position (p_curr_col_name    IN varchar2,
                              p_agg_rows         IN APEX_APPLICATION_GLOBAL.VC_ARR2)
  return integer
  is
    v_relative_position integer;
  begin
    <<aggregated_rows>>
    for i in 1..p_agg_rows.count loop
      if p_curr_col_name = p_agg_rows(i) then        
         return i;
      end if;
    end loop aggregated_rows;
    
    return null;
  end find_rel_position;
  ------------------------------------------------------------------------------
  function get_agg_text(p_curr_col_name   IN varchar2,
                        p_agg_rows        IN APEX_APPLICATION_GLOBAL.VC_ARR2,
                        p_current_row     IN APEX_APPLICATION_GLOBAL.VC_ARR2,
                        p_agg_text        IN varchar2,
                        p_position        IN integer, --start position in sql-query
                        p_col_number      IN INTEGER, --column position when displayed
                        p_default_format_mask     IN varchar2 default null )  
  return clob
  is
    v_tmp_pos       integer;  -- current column position in sql-query 
    v_format_mask   APEX_APPLICATION_PAGE_IR_COMP.computation_format_mask%TYPE;
  begin
      v_tmp_pos := find_rel_position (p_curr_col_name,p_agg_rows); 
      if v_tmp_pos is not null then
        v_format_mask := nvl(get_col_format_mask(get_column_alias_sql(p_col_number)),p_default_format_mask);
--!!!!!!
        --return  get_xmlval(p_agg_text||get_formatted_str(get_current_row(p_current_row,p_position + v_tmp_pos),v_format_mask)||chr(10));
        return  get_xmlval(p_agg_text||to_char(get_current_row(p_current_row,p_position + v_tmp_pos),v_format_mask)||chr(10));
      else
        return  '';
      end if;        
  end get_agg_text;
  
  ------------------------------------------------------------------------------
  function print_aggregate(p_current_row     IN APEX_APPLICATION_GLOBAL.VC_ARR2) 
  return clob
  is
    v_clob            clob;
    v_position        INTEGER;    
  begin
    if l_report.agg_cols_cnt  = 0 then
      return ''; --no aggregate
    end if;    
    v_clob := v_clob || '<AGGREGATE>';
    
    <<visible_columns>>
    FOR i IN l_report.start_with..l_report.end_with loop
      v_position := l_report.end_with; --aggregate are placed after displayed columns and computations
      v_clob := v_clob || bcoll(p_column_alias=>get_column_alias_sql(I));
      --one column cah have only one aggregate of each type
      v_clob := v_clob || get_agg_text(p_curr_col_name => get_column_alias_sql(i),
                                       p_agg_rows      => l_report.sum_columns_on_break,
                                       p_current_row   => p_current_row,
                                       p_agg_text      => ' ',
                                       p_position      => v_position,
                                       p_col_number    => i);
      v_position := v_position + l_report.sum_columns_on_break.count;
      v_clob := v_clob || get_agg_text(p_curr_col_name => get_column_alias_sql(i),
                                       p_agg_rows      => l_report.avg_columns_on_break,
                                       p_current_row   => p_current_row,
                                       p_agg_text      => 'Avgerage: ',
                                       p_position      => v_position,
                                       p_col_number    => i,
                                       p_default_format_mask   => '999G999G999G999G990D000');
      v_position := v_position + l_report.avg_columns_on_break.count;                                       
      v_clob := v_clob || get_agg_text(p_curr_col_name => get_column_alias_sql(i),
                                       p_agg_rows      => l_report.max_columns_on_break,
                                       p_current_row   => p_current_row,
                                       p_agg_text      => 'Max: ',
                                       p_position      => v_position,
                                       p_col_number    => i);
      v_position := v_position + l_report.max_columns_on_break.count;                                 
      v_clob := v_clob || get_agg_text(p_curr_col_name => get_column_alias_sql(i),
                                       p_agg_rows      => l_report.min_columns_on_break,
                                       p_current_row   => p_current_row,
                                       p_agg_text      => 'Min: ',
                                       p_position      => v_position,
                                       p_col_number    => i);
      v_position := v_position + l_report.min_columns_on_break.count;                                 
      v_clob := v_clob || get_agg_text(p_curr_col_name => get_column_alias_sql(i),
                                       p_agg_rows      => l_report.median_columns_on_break,
                                       p_current_row   => p_current_row,
                                       p_agg_text      => 'Median: ',
                                       p_position      => v_position,
                                       p_col_number    => i,
                                       p_default_format_mask   => '999G999G999G999G990D000');
      v_position := v_position + l_report.median_columns_on_break.count;                                 
      v_clob := v_clob || get_agg_text(p_curr_col_name => get_column_alias_sql(i),
                                       p_agg_rows      => l_report.count_columns_on_break,
                                       p_current_row   => p_current_row,
                                       p_agg_text      => 'Count: ',
                                       p_position      => v_position,
                                       p_col_number    => i);
      v_position := v_position + l_report.count_columns_on_break.count;                                 
      v_clob := v_clob || get_agg_text(p_curr_col_name => get_column_alias_sql(i),
                                       p_agg_rows      => l_report.count_distnt_col_on_break,
                                       p_current_row   => p_current_row,
                                       p_agg_text      => 'Count distinct: ',
                                       p_position      => v_position,
                                       p_col_number    => i);
      v_clob := v_clob || ecoll(i);
    end loop visible_columns;
    return  v_clob || '</AGGREGATE>'||chr(10);
  end print_aggregate;    
  ------------------------------------------------------------------------------
  function get_page_items(p_app_id         in number,
                          p_page_id        in number,
                          p_items_list     in varchar2,
                          p_get_page_items in char)
  return clob
  is
    v_clob  clob;    
    v_item_names  APEX_APPLICATION_GLOBAL.VC_ARR2;
  begin
    select item_name
    bulk collect into v_item_names  
    from apex_application_page_items
    where application_id = p_app_id
      and ((page_id = p_page_id and p_get_page_items = 'Y')
          or
          (P_ITEMS_LIST is not null and INSTR(','||P_ITEMS_LIST||',',','||ITEM_NAME||',') >  0))
    union 
    select item_name
    from APEX_APPLICATION_ITEMS
    where application_id = p_app_id  
      and P_ITEMS_LIST is not null 
      and instr(','||p_items_list||',',','||item_name||',') >  0;    
    
    <<items>>
    for i in 1..v_item_names.count loop
     v_clob := v_clob||'<'||upper(v_item_names(i))||'>'
                     ||get_xmlval(v(v_item_names(i)))
                     ||'</'||upper(v_item_names(i))||'>'||chr(10);
    end loop items;
    
    return '<ITEMS>'||chr(10)||v_clob||'</ITEMS>'||chr(10); 
  end get_page_items;  
 
  ------------------------------------------------------------------------------    
  function get_xml_from_ir(p_max_rows in integer)
  return clob
  is
   v_cur         INTEGER; 
   v_result      INTEGER;
   v_colls_count INTEGER;
   v_row         APEX_APPLICATION_GLOBAL.VC_ARR2;
   v_prev_row    APEX_APPLICATION_GLOBAL.VC_ARR2;
   v_columns     APEX_APPLICATION_GLOBAL.VC_ARR2;
   v_current_row number default 0;
   v_xml         CLOB;  
   v_desc_tab    DBMS_SQL.DESC_TAB2;
   v_inside      boolean default false;
  begin
    v_cur := dbms_sql.open_cursor; 
    dbms_sql.parse(v_cur,l_report.report.sql_query,dbms_sql.native);     
    --v_xml := v_xml||'<QUERY>'||get_xmlval(l_report.report.sql_query)||'</QUERY>'||chr(10);
    dbms_sql.describe_columns2(v_cur,v_colls_count,v_desc_tab);    
    --skip internal primary key if need
    if lower(v_desc_tab(1).col_name) = 'apxws_row_pk' then
      l_report.skipped_columns := 1;
    end if;
    l_report.start_with := l_report.skipped_columns + 1 + nvl(l_report.break_really_on.count,0);
    l_report.end_with   := l_report.skipped_columns + nvl(l_report.break_really_on.count,0) + l_report.displayed_columns.count;    
    log('l_report.start_with='||l_report.start_with);
    log('l_report.end_with='||l_report.end_with);
    log('l_report.skipped_columns='||l_report.skipped_columns);
    
    v_xml := v_xml||print_header;
    
    <<bind_variables>>
    for i in 1..l_report.report.binds.count loop
      --remove MAX_ROWS
      if l_report.report.binds(i).name = 'APXWS_MAX_ROW_CNT' then      
        DBMS_SQL.BIND_VARIABLE (v_cur,l_report.report.binds(i).name,p_max_rows);      
        null;
      else
        DBMS_SQL.BIND_VARIABLE (v_cur,l_report.report.binds(i).name,l_report.report.binds(i).value);      
      end if;
    end loop bind_variables;

    <<query_columns>>
    for i in 1..v_colls_count loop
     v_row(i) := '';
     dbms_sql.define_column(v_cur, i, v_row(i),32767);
    end loop query_columns;
    
    v_result := dbms_sql.execute(v_cur);     
    <<main_cycle>>
    LOOP 
         IF DBMS_SQL.FETCH_ROWS(v_cur)>0 THEN          
           -- get column values of the row 
            v_current_row := v_current_row + 1;
            <<query_columns>>
            for i in 1..v_colls_count loop
               DBMS_SQL.COLUMN_VALUE(v_cur, i,v_row(i));                
            end loop;     
            --check control break
            if v_current_row > 1 then
             if is_control_break(v_row,v_prev_row) then                                             
               v_xml := v_xml||'</ROWSET>'||chr(10);
               v_inside := false;
             end if;
            end if;
            if not v_inside then
              v_xml := v_xml||'<ROWSET>'||chr(10);              
              v_xml := v_xml||print_control_break_header(v_row);              
              --print aggregates
              v_xml := v_xml||print_aggregate(v_row);
              v_inside := true;
            END IF;            --            
            <<query_columns>>
            for i in 1..v_colls_count loop
              v_prev_row(i) := v_row(i);                           
            end loop;                 
            v_xml := v_xml||print_row(v_row);            
         ELSE --DBMS_SQL.FETCH_ROWS(v_cur)>0
           EXIT; 
         END IF; 
    END LOOP main_cycle;        
    if v_inside then
       v_xml := v_xml||'</ROWSET>';
       v_inside := false;
    end if;
   dbms_sql.close_cursor(v_cur); 
   
   return v_xml;
  end get_xml_from_ir;
  ------------------------------------------------------------------------------
  function get_final_xml(p_app_id      in number,
                         p_page_id     in number,
                         p_items_list  in varchar2,
                         p_get_page_items in char,
                         p_max_rows       in number)
  return clob
  is
  begin
   return '<?xml version="1.0" encoding="UTF-8"?>'||chr(10)||
          '<DOCUMENT>'||chr(10)||
          get_page_items(p_app_id,p_page_id,p_items_list,p_get_page_items)||
          '<DATA>'||chr(10)||
           get_xml_from_ir(p_max_rows)||
          '</DATA>'||chr(10)||
          '</DOCUMENT>';
  end get_final_xml;
 
  ------------------------------------------------------------------------------
  procedure download_file(p_data        in clob,
                          p_mime_header in varchar2,
                          p_file_name   in varchar2)
  is
    v_blob        blob;
    v_desc_offset PLS_INTEGER := 1;
    v_src_offset  PLS_INTEGER := 1;
    v_lang        PLS_INTEGER := 0;
    v_warning     PLS_INTEGER := 0;   
  begin
        dbms_lob.createtemporary(v_blob,true);
        dbms_lob.converttoblob(v_blob, p_data, dbms_lob.getlength(p_data), v_desc_offset, v_src_offset, dbms_lob.default_csid, v_lang, v_warning);
        sys.htp.init;
        sys.owa_util.mime_header(p_mime_header, FALSE );
        sys.htp.p('Content-length: ' || sys.dbms_lob.getlength( v_blob));
        sys.htp.p('Content-Disposition: attachment; filename="'||p_file_name||'"' );
        sys.owa_util.http_header_close;
        sys.wpg_docload.download_file( v_blob );
        dbms_lob.freetemporary(v_blob);
  end download_file;
  ------------------------------------------------------------------------------
  procedure set_collection(p_collection_name in varchar2,p_data in clob)
  is
   v_tmp char;
  begin
    IF apex_collection.collection_exists (p_collection_name) = FALSE
    THEN
      apex_collection.create_collection (p_collection_name);
    END IF;

   begin
     select '1' --clob001
     into v_tmp
     from apex_collections 
     where collection_name = p_collection_name
        and seq_id = 1;
        
     apex_collection.update_member ( p_collection_name => p_collection_name
                                    , p_seq            => 1
                                    , p_clob001        => p_data);
   exception
     when no_data_found then
      apex_collection.add_member ( p_collection_name => p_collection_name
                                 , p_clob001         => p_data );
       
   end;
  end set_collection;
  ------------------------------------------------------------------------------
  procedure get_report_xml(p_app_id          IN NUMBER,
                           p_page_id         in number,                                
                           p_return_type     IN CHAR DEFAULT 'X', -- "Q" for debug information, "X" for XML-Data
                           p_get_page_items  IN CHAR DEFAULT 'N', -- Y,N - include page items in XML
                           p_items_list      in varchar2,         -- "," delimetered list of items that for including in XML
                           p_collection_name IN VARCHAR2,         -- name of APEX COLLECTION to save XML, when null - download as file
                           p_max_rows        IN NUMBER            -- maximum rows for export                            
                          )
  is
    v_data      clob;
    v_template  clob;    
    v_file       BLOB;
  begin
    --APEX_DEBUG_MESSAGE.ENABLE_DEBUG_MESSAGES(p_level => 7);
    log('p_app_id='||p_app_id);
    log('p_page_id='||p_page_id);
    log('p_return_type='||p_return_type);
    log('p_get_page_items='||p_get_page_items);
    log('p_items_list='||p_items_list);
    log('p_collection_name='||p_collection_name);
    log('p_max_rows='||p_max_rows);
    
    init_t_report(p_app_id,p_page_id);    
    if p_return_type = 'Q' then  -- debug information    
        begin
          v_data := get_final_xml(p_app_id,p_page_id,p_items_list,p_get_page_items,p_max_rows);
          if p_collection_name is not null then  
            set_collection(upper(p_collection_name),v_data);
          end if;
        exception
          when others then
            log('Error in IR_TO_XML.get_report_document '||sqlerrm||chr(10)||chr(10)||dbms_utility.format_error_backtrace);            
        end;
        download_file(v_debug,'text/txt','log.txt');
    elsif p_return_type = 'X' then --XML-Data
        v_data := get_final_xml(p_app_id,p_page_id,p_items_list,p_get_page_items,p_max_rows);
        if p_collection_name is not null then  
          set_collection(upper(p_collection_name),v_data);
        else
          download_file(v_data,'application/xml','report_data.xml');
        end if;
    else
      raise_application_error(-20001,'Unknown parameter p_download_type='||p_return_type);
    end if;
  exception
    when others then 
      raise_application_error(-20001,'Error in IR_TO_XML.get_report_document '||SQLERRM);
  end get_report_xml; 
  
END IR_TO_XML;
/
