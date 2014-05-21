CREATE OR REPLACE PACKAGE IR_TO_XML AS    
  
  -- download interactive report as PDF
  procedure get_report_xml(p_app_id         in number,
                           P_PAGE_ID        in number,                                
                           P_RETURN_TYPE    in char default 'X', -- "Q" for SQL-Query, "X" for XML-Data
                           p_get_page_items in char default 'N', -- Y,N - include page items in XML
                           p_items_list     in varchar2,         -- "," delimetered list of items that for including in XML
                           p_return_item    in varchar2,         -- item name to save XML, when null - download as file
                           p_max_rows       in number            -- maximum rows for export                            
                          );
                              
END IR_TO_XML;
/
