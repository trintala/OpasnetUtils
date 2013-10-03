oprint <- function(x, show_all = FALSE, sortable = TRUE, ...) {

	args <- opbase.parse_args()
	
	if (nrow(x) > 1000 && !show_all)
	{
		print(paste('Showing first 1000 rows out of ',nrow(x),'. Set show_all=TRUE to show all rows.',sep=''))
		x = x[1:1000,]
	}
	
	if (is.null(args$token))
	{
		print(x)
	}
	else
	{
		print_html_safe(args)
		if (sortable)
			print(xtable(x, ...), type = 'html', html.table.attributes="class='wikitable sortable'")
		else
			print(xtable(x, ...), type = 'html', html.table.attributes="class='wikitable'")
	}
}

setGeneric("oprint")

setMethod(
		f = "oprint",
		signature = signature(x = "ovariable"),
		definition = function(x, show_all = FALSE, sortable = TRUE, ...) {
			v = FALSE
			if (ncol(x@output) == 0) x <- EvalOutput(x, verbose = v)
			callGeneric(x@output, show_all = show_all, sortable = sortable, ...)
		}
)

print_html_safe <- function(args){
	cat(paste('<!-- ',digest(paste(args$token,readLines(paste(getwd(),'/../offline/html_safe_key',sep=''),1),sep=''), algo="md5", serialize=FALSE),' -->\n',sep=''))
}
