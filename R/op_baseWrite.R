op_baseWrite <- function(
		dsn,
		input, 
		ident = NULL,
		name = NULL, 
		unit = NULL, 
		objtype_id = NULL, 
		who = NULL, 
		acttype = NULL, 		
		rescol = NULL, 
		n.obs.const = FALSE,
		maxrows = 50000,
		use.utf8 = TRUE, 
		use.utf8.read = TRUE,
		latin1.2.utf8.conv.write = TRUE, 
		utf8.2.latin1.conv.read = TRUE
) { 
	
	if (! is.null(objtype_id))
	{
		obj_types <- list(o1 = 'variable', o2 = 'study', o3 = 'method', o4 = 'assessment', o5 = 'class', o7 = 'nugget', o8 = 'encyclopedia')
		obj_type <- obj_types[[paste('o',objtype_id,sep='')]]
	}
	else
	{
		obj_type <- NULL
	}
	
	if (! is.null(acttype))
	{
		act_types <- list(a4 = 'replace', a5 = 'append')
		act_type <- act_types[[paste('a',acttype,sep='')]]
	}
	else
	{
		act_type <- NULL
	}
	
	return(opbase.upload(input, ident = ident, name = name, obj_type = obj_type, act_type = act_type, unit = unit, who = who, rescol = rescol))
	
}