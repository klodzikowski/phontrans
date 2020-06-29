# This query selects all meaningful data from the log_link_visit action table and unhexes the idvisitor column.
SELECT idlink_va, idsite, conv(hex(idvisitor), 16, 10) as idvisitor, idvisit, server_time, idaction_url, idaction_url_ref, idaction_name, idaction_name_ref, time_spent_ref_action, custom_float FROM piwik_log_link_visit_action;
