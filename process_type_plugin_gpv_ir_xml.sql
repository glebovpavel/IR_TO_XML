set define off
set verify off
set feedback off
WHENEVER SQLERROR EXIT SQL.SQLCODE ROLLBACK
begin wwv_flow.g_import_in_progress := true; end;
/
 
--       AAAA       PPPPP   EEEEEE  XX      XX
--      AA  AA      PP  PP  EE       XX    XX
--     AA    AA     PP  PP  EE        XX  XX
--    AAAAAAAAAA    PPPPP   EEEE       XXXX
--   AA        AA   PP      EE        XX  XX
--  AA          AA  PP      EE       XX    XX
--  AA          AA  PP      EEEEEE  XX      XX
prompt  Set Credentials...
 
begin
 
  -- Assumes you are running the script connected to SQL*Plus as the Oracle user APEX_040200 or as the owner (parsing schema) of the application.
  wwv_flow_api.set_security_group_id(p_security_group_id=>nvl(wwv_flow_application_install.get_workspace_id,2143728413512787));
 
end;
/

begin wwv_flow.g_import_in_progress := true; end;
/
begin 

select value into wwv_flow_api.g_nls_numeric_chars from nls_session_parameters where parameter='NLS_NUMERIC_CHARACTERS';

end;

/
begin execute immediate 'alter session set nls_numeric_characters=''.,''';

end;

/
begin wwv_flow.g_browser_language := 'en'; end;
/
prompt  Check Compatibility...
 
begin
 
-- This date identifies the minimum version required to import this file.
wwv_flow_api.set_version(p_version_yyyy_mm_dd=>'2012.01.01');
 
end;
/

prompt  Set Application ID...
 
begin
 
   -- SET APPLICATION ID
   wwv_flow.g_flow_id := nvl(wwv_flow_application_install.get_application_id,200);
   wwv_flow_api.g_id_offset := nvl(wwv_flow_application_install.get_offset,0);
null;
 
end;
/

prompt  ...ui types
--
 
begin
 
null;
 
end;
/

prompt  ...plugins
--
--application/shared_components/plugins/process_type/gpv_ir_xml
 
begin
 
wwv_flow_api.create_plugin (
  p_id => 2307002152677111 + wwv_flow_api.g_id_offset
 ,p_flow_id => wwv_flow.g_flow_id
 ,p_plugin_type => 'PROCESS TYPE'
 ,p_name => 'GPV_IR_XML'
 ,p_display_name => 'GPV Interactive Report to XML'
 ,p_supported_ui_types => 'DESKTOP'
 ,p_image_prefix => '#PLUGIN_PREFIX#'
 ,p_plsql_code => 
'function gpv_get_xml_from_ir ('||unistr('\000a')||
'    p_process in apex_plugin.t_process,'||unistr('\000a')||
'    p_plugin  in apex_plugin.t_plugin )'||unistr('\000a')||
'    return apex_plugin.t_process_exec_result'||unistr('\000a')||
'is'||unistr('\000a')||
'begin'||unistr('\000a')||
'    IR_TO_XML.get_report_xml(p_app_id            => :APP_ID,'||unistr('\000a')||
'                                p_page_id        => :APP_PAGE_ID,       '||unistr('\000a')||
'                                p_return_type    => p_process.attribute_01,                        '||unistr('\000a')||
' '||
'                               p_get_page_items => p_process.attribute_02,'||unistr('\000a')||
'                                p_items_list     => p_process.attribute_03,'||unistr('\000a')||
'                                p_return_item    => p_process.attribute_04,'||unistr('\000a')||
'                                p_max_rows       => p_process.attribute_05);'||unistr('\000a')||
''||unistr('\000a')||
'  return null;'||unistr('\000a')||
'exception'||unistr('\000a')||
'  when others then'||unistr('\000a')||
'    raise_application_error(-20001,''Error in plugin'||
' "Advanced Printing IR to PDF" :''||SQLERRM);'||unistr('\000a')||
'end gpv_get_xml_from_ir;'
 ,p_execution_function => 'gpv_get_xml_from_ir'
 ,p_substitute_attributes => true
 ,p_subscribe_plugin_settings => true
 ,p_version_identifier => '1.0'
  );
wwv_flow_api.create_plugin_attribute (
  p_id => 2307318887775759 + wwv_flow_api.g_id_offset
 ,p_flow_id => wwv_flow.g_flow_id
 ,p_plugin_id => 2307002152677111 + wwv_flow_api.g_id_offset
 ,p_attribute_scope => 'COMPONENT'
 ,p_attribute_sequence => 1
 ,p_display_sequence => 10
 ,p_prompt => 'Return Data'
 ,p_attribute_type => 'SELECT LIST'
 ,p_is_required => true
 ,p_default_value => 'X'
 ,p_is_translatable => false
 ,p_help_text => 'Choose "Generated XML-data" to return result of Interactive Report in XML format.'||unistr('\000a')||
'Choose "Generated SQL-Query (debug only)" to return generated by Interactive Report SQL-Query.'||unistr('\000a')||
''
  );
wwv_flow_api.create_plugin_attr_value (
  p_id => 2307613927778070 + wwv_flow_api.g_id_offset
 ,p_flow_id => wwv_flow.g_flow_id
 ,p_plugin_attribute_id => 2307318887775759 + wwv_flow_api.g_id_offset
 ,p_display_sequence => 10
 ,p_display_value => 'Generated XML-data'
 ,p_return_value => 'X'
  );
wwv_flow_api.create_plugin_attr_value (
  p_id => 2308010908779393 + wwv_flow_api.g_id_offset
 ,p_flow_id => wwv_flow.g_flow_id
 ,p_plugin_attribute_id => 2307318887775759 + wwv_flow_api.g_id_offset
 ,p_display_sequence => 20
 ,p_display_value => 'Generated SQL-Query (debug only)'
 ,p_return_value => 'Q'
  );
wwv_flow_api.create_plugin_attribute (
  p_id => 2308619929820800 + wwv_flow_api.g_id_offset
 ,p_flow_id => wwv_flow.g_flow_id
 ,p_plugin_id => 2307002152677111 + wwv_flow_api.g_id_offset
 ,p_attribute_scope => 'COMPONENT'
 ,p_attribute_sequence => 2
 ,p_display_sequence => 20
 ,p_prompt => 'Include Page Items '
 ,p_attribute_type => 'CHECKBOX'
 ,p_is_required => false
 ,p_default_value => 'Y'
 ,p_is_translatable => false
 ,p_help_text => 'Include all items that belongs current page with their values in XML. Can be used to display parameters of Interactive Report.'||unistr('\000a')||
'"Item to Return" will be excluded from export.'
  );
wwv_flow_api.create_plugin_attribute (
  p_id => 2308905239858064 + wwv_flow_api.g_id_offset
 ,p_flow_id => wwv_flow.g_flow_id
 ,p_plugin_id => 2307002152677111 + wwv_flow_api.g_id_offset
 ,p_attribute_scope => 'COMPONENT'
 ,p_attribute_sequence => 3
 ,p_display_sequence => 30
 ,p_prompt => 'List of Items to Include'
 ,p_attribute_type => 'PAGE ITEMS'
 ,p_is_required => false
 ,p_is_translatable => false
 ,p_help_text => 'Comma delimeterd list of page or application item to be included in XML.'||unistr('\000a')||
'"Item to Return" is automatically excluded from export.'
  );
wwv_flow_api.create_plugin_attribute (
  p_id => 2309202639874467 + wwv_flow_api.g_id_offset
 ,p_flow_id => wwv_flow.g_flow_id
 ,p_plugin_id => 2307002152677111 + wwv_flow_api.g_id_offset
 ,p_attribute_scope => 'COMPONENT'
 ,p_attribute_sequence => 4
 ,p_display_sequence => 40
 ,p_prompt => 'Item to Return'
 ,p_attribute_type => 'PAGE ITEM'
 ,p_is_required => false
 ,p_is_translatable => false
 ,p_help_text => 'Item where XML-data or SQl-query will be saved. Data greater 32k will be truncated.'||unistr('\000a')||
'When <b>null</b> - result will be downloaded as file. Filesize do not have 32k restriction. '
  );
wwv_flow_api.create_plugin_attribute (
  p_id => 2314527789759271 + wwv_flow_api.g_id_offset
 ,p_flow_id => wwv_flow.g_flow_id
 ,p_plugin_id => 2307002152677111 + wwv_flow_api.g_id_offset
 ,p_attribute_scope => 'COMPONENT'
 ,p_attribute_sequence => 5
 ,p_display_sequence => 50
 ,p_prompt => 'Maximum Rows'
 ,p_attribute_type => 'INTEGER'
 ,p_is_required => true
 ,p_default_value => '1000'
 ,p_is_translatable => false
 ,p_help_text => 'Rows grater this value will be not exported. '||unistr('\000a')||
'To export <b>all rows</b>  set value of 1000000000. '
  );
null;
 
end;
/

commit;
begin
execute immediate 'begin sys.dbms_session.set_nls( param => ''NLS_NUMERIC_CHARACTERS'', value => '''''''' || replace(wwv_flow_api.g_nls_numeric_chars,'''''''','''''''''''') || ''''''''); end;';
end;
/
set verify on
set feedback on
set define on
prompt  ...done
