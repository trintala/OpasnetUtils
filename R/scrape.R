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
 
scrape <- function(page, type, n = NULL, wiki = "", Url ="",
                   firstrow = NULL, assessment=NULL, objectives=NULL) {
  if(type=="discussion") return(scrape.discussion(page=page, n=n, wiki=wiki, Url=Url))
  if(type=="assessment") return(scrape.assessment(assessment=assessment, objectives=objectives))
  if(type=="webtable") return(scrape.webtable(page=page, n=n))
  if(type=="gsheet") return(scrape.gsheet(page=page, firstrow=firstrow))
  stop("type not found.")
}

#' Scrape structured (pragma-dialectical) discussions
#'
#' scrape.discussion is a function that takes a discussion in Opasnet and converts it to a standard graph table.
#' 
#' @param page name of the page to read.
#' @param wiki name of the Opasnet wiki for the page
#' @param Url string for the url of the page to be used as link in insight networks
#' @param n numbers of discussions on the page to scrape. If NULL, all discussions on the page will be read.
#' @return a list of two data.frames. The first has standard headings for a graph table. The second has columns id, type, title, content, sign, target, type, paradigm, relation, and Result; and is ready for creating ovariables from arguments.

scrape.discussion <- function(page, wiki = "", Url="", n=NULL) {
  require(rvest) # html_nodes
  require(xml2)
  require(reshape2)
  
  # Find the previous argument that is one level higher, i.e. the target argument, and the overtarget.
  findtarget <- function(rel, disc, discItem) {
    
    level <- rep(0,nrow(rel))
    for(j in 0:20) {
      level[rel$rel_id %in% html_attr(html_nodes(disc, css=paste(paste(rep("dd", j), collapse=" ")," .argument")),"rel_id")] <- j
    }
    test <- 1:length(level)
    rel$Object <- rep("", length(level)) # Object is target of argument, overtarget is target of Object
    for(j in test) {
      rel$Object[j] <-  rel$Item[max(c(-Inf,test[level == level[j]-1 & test < j]))]
      rel$overtarget[j] <- rel$Item[max(c(-Inf,test[level == level[j]-2 & test < j]))]
    }
    rel$Object[is.na(rel$Object)] <- discItem[1]
    rel$overtarget[is.na(rel$overtarget) & level == 1] <- discItem[1]
    return(rel)
  }
  
  discall <- html_nodes(
    read_html(
      opasnet.page(page,wiki)
    ), css="div.discussion"
  )
  if(is.null(n)) n <- 1:length(discall)
  out <- data.frame()
  for(k in n) { # Go through each discussion
    disc <- discall[[k]]
    
    resd <- html_text(html_nodes(disc, css=".resolved"), trim=TRUE) # Not used
    
    type <- tolower(trimws(substr(html_text(html_nodes(disc, css="font.type")),1,5)))
    type <- gsub("arvok", "value", type)
    type <- gsub("fakta", "fact", type)
    context <- html_attr(disc, "id")
    discItem <- paste(context,c("openingStatement","closingStatement"),sep=".")
    
    # Make unique rel_id for argument-target relations.
    nods <- html_nodes(disc, css=".argumentation .argument")
    
    for(l in 1:length(nods)) {
      xml_attr(nods[[l]], "rel_id") <- l
      nodtmp <- html_nodes(nods[[l]], css=".paradigm")
      xml_attr(nodtmp, "rel_id") <- l
    }
    
    # Make data.frame for opening and closing statements
    
    tmp <- data.frame(
      Context = context,
      rel_id = discItem,
      type = paste(type,c("opening statement","closing statement")),
      Item = discItem,
      label = c(".",html_text(html_nodes(disc, css="b.title"), trim=TRUE)),
      rel = c("produces",""),
      Object = c(discItem[2],""),
      Description = c(
        html_text(html_nodes(disc, css=".openingStatement"), trim=TRUE),
        html_text(html_nodes(disc, css=".closingStatement"), trim=TRUE)),
      URL = paste(Url, context, sep="#"),
      sign = "",
      overtarget = "",
      paradigm = "science",
      stringsAsFactors = FALSE
    )
    out <- rbind(out, tmp)
    
    ######## rel: Find parameters that are unique within the id-target relations
    ######## (one argument may show up several times)
    #   "id"  "title" "type"  "content" "sign"  "target"   "overtarget"
    
    arg_id <- html_attr(html_nodes(disc, css=" .argument"), "id") # argument id
    
    rel <- data.frame(
      Context = context,
      rel_id = html_attr(html_nodes(disc, css=" .argument"), "rel_id"), # relation id
      type = html_text(html_nodes(disc, css=".argument i.type ")),
      Item = arg_id,
      label = html_text(html_nodes(disc, css=" .argument .title")),
      Description = html_text(html_nodes(disc, css=" .argument .content")),
      URL = paste(Url, arg_id, sep="#"),
      sign = html_text(html_nodes(disc, css=" .argument .sign a:first-of-type")),
      #     refs = html_text(html_nodes(nods, css="sup.reference"), trim=TRUE),
      stringsAsFactors = FALSE
    )
    
    rel <- findtarget(rel, disc, discItem) # add Object and overtarget
    
    ######### para: find parameters that are unique in id-target-paradigm triples
    #  "id"        "paradigm"  "relation"  "relevance" "selftruth" "truth"  
    
    para <- data.frame(
      rel_id = html_attr(html_nodes(disc, css=" .argument .paradigm"), "rel_id"),
      paradigm = html_text(html_nodes(disc, css=" .argument .paradigm")),
      rel = html_text(html_nodes(disc, css=" .argument .relation")),
      stringsAsFactors = FALSE
    )
    
    out <- rbind(out, merge(rel,para))
  }
  out$label <- ifelse(out$label %in% c(".",""),substr(out$Description, 1, 30),out$label)
  out <- out[order(out$label, decreasing=TRUE),]
  #  for(i in 1:ncol(out[[1]])) out[[1]][[i]] <- gsub("['\"]", "", out[[1]][[i]])
  
  ###########################################
  # Make inferences about validity and truth based on different paradigms
  
  # Add rows from science paradigm to all other paradigms if missing
  default <- merge(
    unique(out["paradigm"]),
    out[out$paradigm=="science",colnames(out)!="paradigm"]
  )
  default <- rbind(out,default)
  out <- default[!duplicated(default[c("Context","rel_id","paradigm")]),]
  
  # Initial situation: all nodes are true and active
  
  out$true <- TRUE
  out$relevant <- TRUE
  out$active <- TRUE
  
  tmp2 <- out # Temporarily store the data.frame
  
  # Function to test for truth, relevance, activity, orphan, type, and relation
  
  nodetest <- function(dat, type, relation) {
    return(
      dat$type==type & # nodes that are about truth of target
        dat$rel==relation & # nodes that have the relation of interest (usually attack)
        dat$true & # nodes that are true
        dat$active & # nodes that are active
        !dat$Item %in% dat$Object[dat$active] & # nodes that are orphans
        dat$relevant # node-object pairs that are relevant
    )
  }
  
  # Start inference from orphan nodes (nodes that do not have parents)
  out <- data.frame()
  for(i in unique(tmp2$paradigm)) {
    tmp <- tmp2[tmp2$paradigm==i,]
    while(any(tmp$active)) { # Handles threads until all nodes and relations have been handled
      
      # Find nodes that make successful truth attacks
      # Successful truth attacks invalidate their target nodes
      tmp$true[tmp$Item %in% tmp$Object[nodetest(tmp, "truth","attack")]] <- FALSE
      
      # In paradigm personaltine, invalidate all arguments by Tine Bizjak
      if(i=="personaltine") {
        tmp$true[grepl("Tine Bizjak",tmp$sign)] <- FALSE
      }
      
      # Find nodes that make successful relevance attacks
      tst <- nodetest(tmp,"relevance","attack")
      # Successful relevance attacks invalidate their target-overtarget relations
      tmp$relevant[paste(tmp$Item,tmp$Object,sep="-") %in% paste(tmp$Object,tmp$overtarget,sep="-")[tst]] <- FALSE
      
      # Remove orphan nodes from active nodes
      tmp$active[!tmp$Item %in% tmp$Object[tmp$active]] <- FALSE
      
    }
    out <- rbind(out, tmp)
  }
  
  # Post-processing: adjust type and rel so that graphs plot correctly
  tst <- out$type %in% c("truth","relevance")
  out$type[tst] <- ifelse(out$true, "true statement", "false statement")[tst]
  out$rel[tst] <- paste(ifelse(out$relevant,"relevant","irrelevant"),out$rel)[tst]
  
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
  assessment=NULL,
  objectives = character()
) {
  #  require(DiagrammeR)
  require(OpasnetUtils)
  nod <- data.frame()
  ova <- character()
  dec <- character()
  dat <- character()
  plo <- character()
  if(is.null(assessment)) {
    dep <- character()
    for(i in ls(envir= .GlobalEnv)) {
      if("ovariable" %in% class(get(i))) dep <- c(dep, i)
    }
    if(length(dep)==0) stop("No ovariables found!")
    dep <- data.frame(Name = dep)
    URLass <- ""
  } else {
    dep <- assessment@dependencies
    URLass <- assessment@meta$wiki_page_id
  }
  
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
  if(!is.null(assessment)) nod$type[nod$Item %in% assessment@name] <- "assessment"
  
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
