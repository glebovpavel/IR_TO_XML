CREATE OR REPLACE PACKAGE BIP_TO_FOP AS 

 procedure do_convert(P_TEMPLATE_ID IN BIP_TO_FOP_TEMPLATES.TEMPLATE_ID%TYPE);

 procedure export_template(P_TEMPLATE_ID IN BIP_TO_FOP_TEMPLATES.TEMPLATE_ID%TYPE,
                           p_layout_id   in number);

 
END BIP_TO_FOP;
/


CREATE OR REPLACE PACKAGE BODY BIP_TO_FOP AS 
  --grant update on APEX_040200.WWV_FLOW_REPORT_LAYOUTS to gpv
  --create view APEX_REPORT_LAYOUTS as select * from APEX_040200.WWV_FLOW_REPORT_LAYOUTS

 --http://13ter.info/blog/?p=364
 FUNCTION blob_to_clob (p_in blob) RETURN clob IS
    v_clob    clob;
    v_varchar VARCHAR2(32767);
    v_start   PLS_INTEGER := 1;
    v_buffer  PLS_INTEGER := 32767;   
  BEGIN
    dbms_lob.createtemporary(v_clob, TRUE);
    FOR i IN 1..CEIL(dbms_lob.getlength(p_in) / v_buffer)
    LOOP
      v_varchar := utl_raw.cast_to_varchar2(dbms_lob.SUBSTR(p_in, v_buffer, v_start));
      dbms_lob.writeappend(v_clob, LENGTH(v_varchar), v_varchar);
      v_start := v_start + v_buffer;
    END LOOP;
    RETURN v_clob;
  END;
 -------------------------------------------------------------------------------
 FUNCTION clob_to_blob (p_in clob) RETURN blob IS 
    v_blob        blob;
    v_desc_offset PLS_INTEGER := 1;
    v_src_offset  PLS_INTEGER := 1;
    v_lang        PLS_INTEGER := 0;
    v_warning     PLS_INTEGER := 0;  
 BEGIN
    dbms_lob.createtemporary(v_blob,TRUE);
    dbms_lob.converttoblob(v_blob, p_in, dbms_lob.getlength(p_in), v_desc_offset, v_src_offset, dbms_lob.default_csid, v_lang, v_warning);
    RETURN v_blob;
 END;
 -------------------------------------------------------------------------------
 procedure convert_fo_template(p in out nocopy clob) 
 is
   v_region_before  varchar2(3000);
   v_region_body    varchar2(3000);
   v_region_after   varchar2(3000);
 begin
    -- tested with Oracle BI Publisher Desctop 11.1.1.6.2 Build 11.116.2.1
    
    -- del incompatible strings
    p:= replace(p,'<xsl:variable name="_XDOXSLTCTX" select="xdoxslt:set_xslt_locale($_XDOCTX, $_XDOLOCALE, $_XDOTIMEZONE, $_XDOCALENDAR, $_XDODFOVERRIDE, $_XDOCURMASKS, $_XDONFSEPARATORS)"/>','');
    p:= replace(p,'<fo:title>','');
    p:= replace(p,'<xsl:value-of select="xdoxslt:one($titlevar)" xdofo:field-name="$titlevar"/>','');
    p:= replace(p,'</fo:title>','');
    
    --replace xdoxslt:one() => number()
    p:= replace(p,'xdoxslt:one','number');

    -- del  xdofo: attributes
    -- <fo:root xdofo:nf-separator="{$_XDONFSEPARATORS}"> => <fo:root >    
    p := regexp_replace(p,'xdofo:(\w|[-])+="[^"]+"','');
    -- del xdofo: open and close tags
    -- <xdofo:property name="default-tab-width">35.4pt</xdofo:property> => NULL
    p := regexp_replace(p,'<xdofo:[^/]+/xdofo:(\w|[-])+>','');
    -- del standalone xdofo: tags
    -- <xdofo:properties> = > NULL
    p := regexp_replace(p,'</?xdofo:(\w|[-])+>','');
    --del empty <fo:static-content/>
    p := regexp_replace(p,'<fo:static-content[^>]+/>','');
    
    --remove other incompatible attributes
    p := regexp_replace(p,'style\-name="[^"]+"','');
    p := regexp_replace(p,'font\-family\-generic="[^"]+"','');
    p := regexp_replace(p,'style\-id="[^"]+"','');
    p := regexp_replace(p,'xml:space="[^"]+"','');

    /*
               <fo:region-before region-name="region-header" extent="35.45pt"/>
               <fo:region-body region-name="region-body" margin-top="35.45pt" margin-bottom="21.3pt"/>
               <fo:region-after region-name="region-footer" extent="21.3pt" display-align="after"/>
         =>
               <fo:region-body region-name="region-body" margin-top="35.45pt" margin-bottom="21.3pt"/>
               <fo:region-before region-name="region-header" extent="35.45pt"/>               
               <fo:region-after region-name="region-footer" extent="21.3pt" display-align="after"/>
   */   
   v_region_before  := regexp_substr(p,'<fo:region\-before .+/>');
   v_region_body    := regexp_substr(p,'<fo:region\-body .+/>');
   v_region_after   := regexp_substr(p,'<fo:region\-after .+/>');
   p := replace(p,v_region_before,'');
   p := replace(p,v_region_body,'');
   p := replace(p,v_region_after,v_region_body||v_region_before||v_region_after);
   
   --remove empty rows
   p := regexp_replace(p,'<fo:table-row>[^<]*<fo:table-cell [^/]*/>[^<]*</fo:table-row>','');  
   
 end convert_fo_template;
 -------------------------------------------------------------------------------
 procedure do_convert(P_TEMPLATE_ID IN BIP_TO_FOP_TEMPLATES.TEMPLATE_ID%TYPE)
 is
  v_blob         blob;
  v_clob         clob;
  v_tmp          integer;
 begin
     dbms_lob.createtemporary(v_clob, TRUE);
     for i in (select * from BIP_TO_FOP_TEMPLATES where TEMPLATE_ID = P_TEMPLATE_ID for update) loop
       v_tmp := 1;       
       v_clob := BIP_TO_FOP.blob_to_clob(i.BIP_BLOB);
       BIP_TO_FOP.convert_fo_template(v_clob);       
       v_tmp := 4;                       
       v_blob := BIP_TO_FOP.clob_to_blob(v_clob);
       update BIP_TO_FOP_TEMPLATES set FOP_BLOB = v_blob where TEMPLATE_ID = P_TEMPLATE_ID;
     end loop;  
 exception
   when others then
     raise_application_error(-20001,SQLERRM||' v_tmp='||v_tmp);
 end do_convert;
 -------------------------------------------------------------------------------
 procedure export_template(P_TEMPLATE_ID IN BIP_TO_FOP_TEMPLATES.TEMPLATE_ID%TYPE,
                           p_layout_id   in number)                           
 is
  v_clob         clob;
 begin
   dbms_lob.createtemporary(v_clob, TRUE);
   for i in (select * from BIP_TO_FOP_TEMPLATES where TEMPLATE_ID = P_TEMPLATE_ID for update) loop
     v_clob := BIP_TO_FOP.blob_to_clob(i.FOP_BLOB);
     update APEX_REPORT_LAYOUTS set page_template = v_clob where ID = p_layout_id;
   end loop;
 end export_template;
 
 
END BIP_TO_FOP;
/
