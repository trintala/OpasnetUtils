#' Wrapper function for scraping data from various sources.
#' 
#' scrape can be used for scraping data from discussions, assessments, webtables, or google sheets.
#' @param page url for the page to be scraped.
#' @param type type of function to use. Possible values: "discussion","assessment","webtable","gsheet"
#' @param n number(s) of the tables or discussions on the page to scrape.
#' @param firstrow atom vector with the number of the first data row on the gsheet
#' @param assessment ovariable that contains other assessment objects as dependencies
#' @param objectives names of ovariables that are objectives in the model
#' @return data.frame of standard type
 
scrape <- function(page, type, n, firstrow, assessment, objectives) {
  if(type=="discussion") return(scrape.discussion(page, n)[[1]])
  if(type=="assessment") return(scrape.assessment(assessment, objectives))
  if(type=="webtable") return(scrape.webtable(page, n))
  if(type=="gsheet") return(scrape.gsheet(page, firstrow))
  stop("type not found.")
}

#' Scraping structured discussions from the web
#' 
#' scrape.discussion is a function that takes a discussion in Opasnet and converts it to a standard graph table.
#' 
#' @param page URL for the page to read.
#' @param n number of discussion on the page. If NULL, all discussions on the page will be read.
#' @return a list of two data.frames. The first has standard headings for a graph table. The second has columns id, type, title, content, sign, target, type, paradigm, relation, and Result; and is ready for creating ovariables from arguments.

scrape.discussion <- function(page, n=NULL) {
  require(rvest)
  require(xml2)
  require(reshape2)
  discall <- html_nodes(read_html(page), css="div.discussion")
  if(is.null(n)) n <- 1:length(discall)
  out <- list(data.frame(),data.frame())
  for(k in n) {
    disc <- discall[[k]]
    stat <- html_text(html_nodes(disc, css=".statements"), trim=TRUE)
    reso <- html_text(html_nodes(disc, css=".resolution"), trim=TRUE)
    resd <- html_text(html_nodes(disc, css=".resolved"), trim=TRUE)
    title<- html_text(html_nodes(disc, css="b.title"), trim=TRUE)
    type <- tolower(trimws(substr(html_text(html_nodes(disc, css="font.type")),1,5)))
    type <- gsub("arvok", "value", type)
    type <- gsub("fakta", "fact", type)
    id <- html_attr(disc, "id")
    disc.id <- paste(id, c("statement","resolution"),sep=".")
    
    # Make argument and paradigm id's specific to argument-target pairs.
    nods <- html_nodes(disc, css=".argumentation .argument")
    tmp <- html_attr(nods,"id")
    tmp <- paste(tmp, 1:length(tmp),sep="£££")
    xml_attrs(nods) <- lapply(tmp, function(x) {c(class="argument",id=x)})
    for(l in 1:length(tmp)) {
      nodtmp <- html_nodes(nods[[l]], css=".paradigm")
      xml_attrs(nodtmp) <- rep(list(c(id=tmp[l], class="paradigm")), length(nodtmp))
    }
    
    tmp <- data.frame(
      Oldid = disc.id,
      type = paste(type,c("statement","resolution")),
      Item = disc.id,
      label = c(".",title),
      Relation = c("produces",""),
      Object = c(disc.id[2],""),
      Description = c(stat, reso),
      URL = paste(page, id, sep="#"),
      stringsAsFactors = FALSE
    )
    if(nrow(out[[1]])==0) out[[1]] <- tmp else out[[1]] <- orbind(out[[1]], tmp)
    
    ######## arg: Find parameters that are unique within the id-target pairs
    #   "id"  "title" "type"  "content" "sign"  "target"   "overtarget"
    
    arg <- data.frame(
      id = html_attr(html_nodes(disc, css=" .argument"), "id"),
      title = html_text(html_nodes(disc, css=" .argument .title")),
      type = html_text(html_nodes(disc, css=".argument i.type ")),
      content = html_text(html_nodes(disc, css=" .argument .content")),
      sign = html_text(html_nodes(disc, css=" .argument .sign a:first-of-type")),
      stringsAsFactors = FALSE
    )
    
    # Find the previous argument that is one level higher, i.e. the target argument, and the overtarget.
    findtarget <- function(arg, disc) {
      
      level <- rep(0,nrow(arg))
      for(j in 0:8) {
        level[arg$id %in% html_attr(html_nodes(disc, css=paste(paste(rep("dd", j), collapse=" ")," .argument")),"id")] <- j
      }
      test <- 1:length(level)
      arg$target <- rep("", length(level))
      for(j in test) {
        arg$target[j] <-  arg$id[max(c(-Inf,test[level == level[j]-1 & test < j]))]
        arg$overtarget[j] <- arg$id[max(c(-Inf,test[level == level[j]-2 & test < j]))]
      }
      arg$target[is.na(arg$target)] <- disc.id[1]
      arg$overtarget[is.na(arg$overtarget) & level == 1] <- disc.id[1]
      return(arg)
    }
    
    arg <- findtarget(arg, disc)
    
    ######### targ: find parameters that are unique in id-target-paradigm triples
    #  "id"        "paradigm"  "relation"  "relevance" "selftruth" "truth"  
    
    targ <- data.frame(
      id = html_attr(html_nodes(disc, css=" .argument .paradigm"), "id"),
      paradigm = html_text(html_nodes(disc, css=" .argument .paradigm")),
      relation = html_text(html_nodes(disc, css=" .argument .relation")),
      relevance = html_attr(html_nodes(disc, css=" .argument .relation"), "color"),
      selftruth = html_attr(html_nodes(disc, css=" .argument .selftruth"), "color"),
      stringsAsFactors = FALSE
    )
    
    targ$relation  <- c("attack","defense","comment","branch")[match(substr(targ$relation,1,3), c("\U21E4--","\U2190--","---","--\U2192"))]
    targ$truth     <- ifelse(targ$relevance=="gray",0,1)
    targ$relevance <- ifelse(targ$relevance=="gray",0,1)
    targ$selftruth <- ifelse(targ$selftruth=="gray",0,1)
    targ <- findtarget(targ, disc)[-8] # Remove redundant overtarget
    
    ### Create another data.frame with full argument structure for ovariable formation
    
    tmp <- arg # Add rows for selftruth
    tmp$type <- "selftruth"
    tmp <- rbind(arg,tmp)
    tmp <- tmp[!duplicated(arg$id, arg$type) , ]
    
    arg2 <- melt(
      targ,
      measure.vars = c("relevance","truth","selftruth"),
      variable.name="type",
      value.name="Result"
    )
    arg2 <- merge(tmp, arg2)
    out[[2]] <- rbind(out[[2]], arg2)
    
    # Produce a vector with the same nrow as arg but all paradigms combined.
    
    arg1 <- merge(arg,targ)
    selftruth <- c("false","true")[arg1$selftruth+1]
    tmp <- sapply(tapply(
      paste(arg1$paradigm, selftruth, sep=": "),
      arg1["id"], paste),function(x) paste(x, collapse="; "))
    tmp <- data.frame(id = names(tmp), par = tmp, stringsAsFactors = FALSE)
    arg1 <- merge(arg1,tmp) # Merge instead of cbind in case tapply mixes the order of arguments.
    arg1$id <- gsub("£££.*", "", arg1$id) # Replace argument-target-specific id's again with argument id's.
    arg1$target <- gsub("£££.*", "", arg1$target)
    
    out[[1]] <- orbind(out[[1]], data.frame(  ### Arguments
      Oldid = paste(id,arg1$id,sep="."),
      type = paste(selftruth,"argument"),
      Item = arg1$id,
      label = arg1$title,
      Relation = paste(c("irrelevat","relevant")[arg1$relevance+1], arg1$relation),
      Object = arg1$target,
      Description = paste(arg1$content, "| paradigms:", arg1$par),
      URL = paste(page, arg1$id,sep="#"),
      stringsAsFactors = FALSE
    ))
  }
  out[[1]]$label <- ifelse(out[[1]]$label==".",substr(out[[1]]$Description, 1, 30),out[[1]]$label)
  for(i in 1:ncol(out[[1]])) out[[1]][[i]] <- gsub("['\"]", "", out[[1]][[i]])
  
  return(out)
}

#' Scrape data from google sheets
#' 
#' scrape.gsheet is a function for scraping google sheets and making data.frames.
#' 
#' @param page character vector with the URL of the google sheet
#' @param firstrow atom vector with the number of the first data row on the gsheet
#' @return a data.frame

scrape.gsheet <- function(page, firstrow) {
  require(gsheet)
  
  out <- gsheet2tbl(page)
  if(firstrow>2) out <- out[-(1:(firstrow-2)) , ]
  return(out)
}

#' Scrape normal web tables
#' 
#' scrape.webpage is a function for scraping data from a table on a webpage.
#' 
#' @param page URL of the page to be scraped.
#' @param n number of table on the page to be scraped.
#' @return a data.frame

scrape.webtable <- function(page, n) {
  require(rvest)
  out <- html_table(read_html(page),fill=TRUE)[[n]]
  return(out)
}

#' Create URL for a data source items
#' 
#' Creates a unique URL based on wiki page id and object name. Used to create hyperlinks to the knowledge crystal pages.
#' 
#' @param page wiki_page_id: character vector format "Op_en7748" 
#' @param name name of the knowledge crystals: character vector
#' @return Character vector of URLs

makeurl <- function( 
  page, 
  name 
) {
  if(is.null(page)|is.null(name)) return(NA) else
    out <- paste0(
      c(
        en = "http://en.opasnet.org/w/index.php?curid=",
        fi = "http://fi.opasnet.org/fi/index.php?curid="
      )[substr(page, 4,5)], # is it en or fi?
      substr(page, 6,11), # id
      "#", gsub(" ", "_", name)
    )
  out <- ifelse(is.na(page)|is.na(name), NA, out)
  return(out)
}

#' Scrape ovariables and other objects in an assessment
#' 
#' scrape.assessment makes standard data.frame for insight diagram out of all ovariables, odecisions, and data.frames and their dependencies in the global environment.
#' 
#' @param assessment ovariable that contains other assessment objects as dependencies
#' @param objectives names of ovariables that are objectives in the model
#' @return a list of two data.frames. The first one is for making insight diagrams, the second for making discussion ovariables for analysis (the latter part not implemented yet)

scrape.assessment <- function(
  assessment,
  objectives = character()
) {
  #  require(DiagrammeR)
  require(OpasnetUtils)
  nod <- data.frame()
  ova <- character()
  dec <- character()
  dat <- character()
  plo <- character()
  dep <- assessment@dependencies
  URLass <- assessment@meta$wiki_page_id
  
  #### Find all objects (decisions, ovariables, data.frames and graphs)
  objs <- list()
  for(i in dep$Name) {
    cl <- class(get(i))
    if("ovariable" %in% cl) ova <- c(ova, i)
    if("odecision" %in% cl) dec <- c(dec, i)
    if("data.frame" %in% cl) dat <- c(dat, i)
    if(any(c("ggplot","dgr_graph") %in% cl)) plo <- c(plo, i)
    if(is.vector(get(i))) objs <- c(objs, list(get(i))) else objs <- c(objs, get(i))
  }
  names(objs) <- dep$Name
  
  ###### Add all decisions and options
  
  for(i in dec) { 
    deci <- objs[[i]]@dectable
    if(nrow(deci)>0) {
      tst <- !duplicated(deci[c("Option","Decision")])
      nod <- rbind(
        nod,
        data.frame(
          #          Oldid = paste("Opt", match(i, dec), sep=""),
          type = "option",
          Item = as.character(deci$Option[tst]),
          rel = "is option for",
          Object = as.character(deci$Decision[tst]),
          Description = if(is.null(deci$Description)) NA else
            as.character(deci$Description[tst]),
          URL = makeurl(URLass, deci$Option)[tst],
          stringsAsFactors = FALSE
        )
      )
      
      tst <- !duplicated(deci[c("Decision","Variable")])
      nod <- rbind(
        nod,
        data.frame(
          #          Oldid = paste("Dec", match(i, dec), sep=""),
          type = "decision",
          Item = as.character(deci$Decision[tst]),
          rel = "affects",
          Object = as.character(deci$Variable[tst]),
          Description = if(is.null(deci$Description)) NA else
            as.character(deci$Description[tst]),
          URL = makeurl(URLass, deci$Decision)[tst],
          stringsAsFactors = FALSE
        )
      )
    }
  }
  
  ####### Add all ovariables
  
  for(i in ova) {
    obj <- objs[[i]]
    nam <- if(length(obj@name)>0) obj@name else ""
    nod <- rbind( # Add dependencies
      nod,
      data.frame(
        #        Oldid = paste("Ova",match(i,ova),sep=""),
        type = "ovariable",
        Item = nam,
        rel = "is affected by",
        Object = if(nrow(obj@dependencies)==0) NA else as.character(obj@dependencies$Name),
        Description = if(is.null(obj@meta$Description)) NA else
          obj@meta$Description,
        URL = makeurl(obj@meta$wiki_page_id, nam),
        stringsAsFactors = FALSE
      )
    )
    tmp <- colnames(obj@output)[  # Add indices
      obj@marginal &
        !grepl("Source$", colnames(obj@output)) &
        ! colnames(obj@output) %in% c("Iter") # deci not tested because of error
      ]
    if(length(tmp)>0) {
      nod <- rbind(
        nod,
        data.frame(
          #          Oldid = paste0("Ova",match(i,ova),tmp),
          type = "index",
          Item = tmp,
          rel = "is index for",
          Object = nam,
          Description = NA,
          URL = makeurl(URLass, tmp),
          stringsAsFactors = FALSE
        )
      )
    }
  }
  
  ### Add graphs
  
  if(length(plo)>0) {
    nod <- orbind(
      nod,
      data.frame(
        #        Oldid = paste0("Plo", 1:length(plo)),
        type = "graph",
        Item = plo,
        stringsAsFactors = FALSE
      )
    )
  }
  
  # V(dag)$Size[V(dag)$name == i] <- nrow(obj@output)     # Size
  # vertex.size = log(V(dag)$Size)+2, # Vertex size is (non-linearly) relative to rows in output.
  
  ### Add data.frames 
  
  if(length(dat)>0) {
    nod <- orbind(
      nod,
      data.frame(
        #        Oldid = paste("Dat", 1:length(dat), sep=""),
        type = "data",
        Item = dat,
        stringsAsFactors = FALSE
      )
    )
  }
  
  # Retype objectives and assessments if available
  nod$type[nod$Item %in% objectives] <- "objective"
  nod$type[nod$Item %in% assessment@name] <- "assessment"
  
  # Add info from Page column if available and doesn't exist already
  if(!is.null(dep$Page)) {
    tst <- is.na(nod$Description)
    nod$Description[tst] <- as.character(dep$Description[match(nod$Item, dep$Name)][tst])
    tst <- is.na(nod$URL)
    nod$URL[tst] <- makeurl(
      dep$Page[match(nod$Item[tst], dep$Name)],
      dep$Name[match(nod$Item[tst], dep$Name)]
    )
  }
  tst <- is.na(nod$Description)
  nod$Description[tst] <- nod$type[tst]
  
  #Add info (with force) from type column in dep if available
  if(!is.null(dep$type)) {
    tst <- dep$Name[!(is.na(dep$type) | dep$type=="")]
    tst <- tst[tst %in% nod$Item] # Necessary because some names may be missing
    nod$type[match(tst,nod$Item)] <- as.character(dep$type[match(tst,dep$Name)])
  }
  
  #Add info (with force) from label column in dep if available
  nod$label <- NA # Add column anyway because makeInsightTables requires standard columns
  if(!is.null(dep$label)) {
    tst <- dep$Name[!(is.na(dep$label) | dep$label=="")]
    tst <- tst[tst %in% nod$Item] # Necessary because some names may be missing
    nod$label[match(tst,nod$Item)] <- as.character(dep$label[match(tst,dep$Name)])
  }
  
  # Add relations defined by hand on assessment@dependencies
  tst <- dep$Name[!(is.na(dep$Child) | dep$Child=="")]
  if(length(tst)>0) { # Does nothing if column Child does not exist
    nod <- orbind(
      nod,
      data.frame(
        #        Oldid = as.character(nod$Oldid[match(tst,nod$Item)]),
        Item = as.character(tst),
        rel = c(
          rep("indirectly affects",3),
          "is data for",
          "describes"
        )[match(nod$type[match(tst,nod$Item)],c("ovariable","key ovariable","objective","data","graph"))],
        Object = as.character(dep$Child[match(tst,dep$Name)])
      )
    )
  }
  return(nod) 
}
