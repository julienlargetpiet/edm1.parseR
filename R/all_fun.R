#' read_edm_parser
#'
#' Allow to read data from edm parsed dataset, see examples
#'
#' @param inpt is the input dataset
#' @param to_find_v is the vector containing the path to find the data, see examples
#'
#' @examples
#'
#' print(read_edm_parser("(ok(ee:56))(ok(oui(rr((rr2:6)(rr:5))))(oui(bb(rr2:1)))(ee1:4))", 
#' to_find_v = c("ok", "oui", "rr", "rr2")))
#'
#' [1] "6"
#'
#' print(read_edm_parser("(ok(ee:56))(ok(oui(rr((rr2:6)(rr:5))))(oui(bb(rr2:1)))(ee1:4))", to_find_v = c("ok", "ee")))
#'
#' [1] "56"
#'
#' print(read_edm_parser("(ok(ee:56))(ok(oui(rr((rr2:6)(rr:5))))(oui(bb(rr2:1)))(ee1:4))", to_find_v = c("ee")))
#' 
#' [1] "56"
#'
#' @export

read_edm_parser <- function(inpt, to_find_v = c()){
  dynamic_idx_convertr <- function(from_v_ids, from_v_val){
    lngth <- length(from_v_ids)
    i = 1
    no_stop <- TRUE
    I = 1
    lst_nchar = 0
    while (I <= length(from_v_val) & no_stop){
      while ((nchar(from_v_val[I]) + lst_nchar) >= from_v_ids[i] & no_stop){
        from_v_ids[i] <- I
        if (i == length(from_v_ids)){ no_stop <- FALSE }else{ i = i + 1 }
      }
      lst_nchar = lst_nchar + nchar(from_v_val[I])
      I = I + 1
    }
    return(from_v_ids)
  }
  pairs_findr <- function(inpt, ptrn1="(", ptrn2=")"){
          
        regex_spe_detect <- function(inpt){
                fillr <- function(inpt_v, ptrn_fill="\\.\\.\\.\\d"){
                  ptrn <- grep(ptrn_fill, inpt_v)
                  while (length(ptrn) > 0){
                    ptrn <- grep(ptrn_fill, inpt_v)
                    idx <- ptrn[1] 
                    untl <- as.numeric(c(unlist(strsplit(inpt_v[idx], split="\\.")))[4]) - 1
                    pre_val <- inpt_v[(idx - 1)]
                    inpt_v[idx] <- pre_val
                    if (untl > 0){
                      for (i in 1:untl){
                        inpt_v <- append(inpt_v, pre_val, idx)
                      }
                    }
                  ptrn <- grep(ptrn_fill, inpt_v)
                  }
                  return(inpt_v)
                }
                inpt <- unlist(strsplit(x=inpt, split=""))
                may_be_v <- c("[", "]", "{", "}", "-", "_", ".", "(", ")", "/", "%", "*", "^", "?", "$")
                pre_idx <- unique(match(x=inpt, table=may_be_v))
                pre_idx <- pre_idx[!(is.na(pre_idx))]
                for (el in may_be_v[pre_idx]){
                        cnt = 0
                        for (i in grep(pattern=paste("\\", el, sep=""), x=inpt)){
                                inpt <- append(x=inpt, values="\\", after=(i - 1 + cnt))
                                cnt = cnt + 1
                        }
                }
                  return(paste(inpt, collapse=""))
          }
  
          lst <- unlist(strsplit(x=inpt, split=""))
  
          lst_par <- c()
  
          lst_par_calc <- c()
  
          lst_pos <- c()
  
          paires = 1
  
          pre_paires = 1
  
          pre_paires2 = 1
  
          if ((length(grep(x=lst, pattern=regex_spe_detect(inpt=ptrn1))) * 2) > 0){
  
                  for (i in 1:(length(grep(x=lst, pattern=regex_spe_detect(inpt=ptrn1))) * 2)){ 
  
                          lst_par <- c(lst_par, 0)
  
                          lst_par_calc <- c(lst_par_calc, 0)
  
                          lst_pos <- c(lst_pos, 0)
  
  
                  }
  
          }
  
          vec_ret <- c()
  
          par_ = 1
  
          lvl_par = 0
  
          for (el in 1:length(lst)){
  
             if (lst[el] == ptrn1){
  
                     if (!(is.null(vec_ret))){
  
                             lst_par_calc[pre_paires2:pre_paires][-vec_ret] <- lst_par_calc[pre_paires2:pre_paires][-vec_ret] + 1
  
                     }else{
  
                             lst_par_calc[pre_paires2:pre_paires] <- lst_par_calc[pre_paires2:pre_paires] + 1
  
                     }
  
                     pre_paires = pre_paires + 1
  
                     pre_cls <- TRUE
  
                     lst_pos[par_] <- el
  
                     par_ = par_ + 1
  
                     lvl_par = lvl_par + 1
  
             }
  
             if (lst[el] == ptrn2){
  
                     lvl_par = lvl_par - 1
  
                     if (!(is.null(vec_ret))){
  
                          lst_par_calc[c(pre_paires2:pre_paires)][-vec_ret] <- lst_par_calc[pre_paires2:pre_paires][-vec_ret] - 1
  
                          pre_val <- lst_par_calc[pre_paires2:pre_paires][vec_ret]
  
                          lst_par_calc[pre_paires2:pre_paires][vec_ret] <- (-2)
                     
                     }else{
  
                          lst_par_calc[c(pre_paires2:pre_paires)] <- lst_par_calc[pre_paires2:pre_paires] - 1
  
                     }
  
                     if (!(is.null(vec_ret))){ 
  
                             pre_mtch <- match(x=c(0, -1), table=lst_par_calc[pre_paires2:pre_paires][-vec_ret])
  
                             lst_par_calc[pre_paires2:pre_paires][vec_ret] <- pre_val 
  
                     }else{
  
                             pre_mtch <- match(x=c(0, -1), table=lst_par_calc[pre_paires2:pre_paires])
  
                     }
  
                     cnt_par = 1
  
                     cnt2 = 0
  
                     if (!(is.null(vec_ret))){
  
                             vec_ret <- sort(vec_ret)
  
                             if (pre_mtch[1] >= min(vec_ret)){
  
                                  cnt2 = 2
  
                                  while (pre_mtch[1] > cnt_par & cnt2 <= length(vec_ret)){
  
                                          if ((vec_ret[cnt2] - vec_ret[(cnt2 - 1)]) > 1){
  
                                                  cnt_par = cnt_par + (vec_ret[cnt2] - vec_ret[(cnt2 - 1)]) - 1
  
                                          }
  
                                          cnt2 = cnt2 + 1
  
                                  }
  
                                  if (pre_mtch[1] > cnt_par){
  
                                          cnt_par = length(vec_ret) / 2 + 1
  
                                  }
  
                                  cnt2 = cnt2 - 1
  
                             }
  
                     }
  
                     lst_par[pre_mtch[1] + (pre_paires2 - 1) + ifelse(cnt2 %% 2 == 0, cnt2, (cnt2 - 1))] <- paires 
  
                     lst_par[pre_mtch[2] + (pre_paires2 - 1) + length(vec_ret)] <- paires 
  
                     if ((pre_mtch[1] + (pre_paires2 - 1)) == 1){
  
                          pre_paires2 = pre_mtch[2] + (pre_paires2 - 1) + length(vec_ret) + 1
  
                          vec_ret <- c()
  
                          cnt_par = 0
  
                     } else if (lst_par_calc[(pre_mtch[1] + (pre_paires2 - 1) - 1)] == -1 & ifelse(is.null(vec_ret), TRUE, 
                                  is.na(match(x=-1, table=lst_par_calc[pre_paires2:pre_paires][-vec_ret])))){
  
                          pre_paires2 = pre_mtch[2] + (pre_paires2 - 1) + length(vec_ret) + 1
  
                          vec_ret <- c()
  
                          cnt_par = 0
  
                     } else{
  
                          vec_ret <- c(vec_ret, (pre_mtch[1]) + ifelse(cnt2 %% 2 == 0, cnt2, (cnt2 - 1)), 
                                       (pre_mtch[2] + length(vec_ret)))
  
                     }
  
                     paires = paires + 1
  
                     pre_paires = pre_paires + 1
  
                     pre_cls <- FALSE
  
                     lst_pos[par_] <- el
  
                     par_ = par_ + 1
  
             }
  
          }
  
          return(list(lst_par, lst_pos))
  
  }
  better_split_any <- function(inpt, split_v = c()){
    glue_groupr_v <- function(inpt_v, group_v = c(), untl){
      rtn_v <- c()
      cur_v <- c()
      grp_status <- FALSE
      cnt_untl = 0
      for (el in inpt_v) {
        if (el %in% group_v & cnt_untl < untl){
          grp_status <- TRUE
          cur_v <- c(cur_v, el)
          cnt_untl = cnt_untl + 1
        }else if (grp_status){
          grp_status <- FALSE
          rtn_v <- c(rtn_v, paste(cur_v, collapse = ""))
          cur_v <- c()
          if (el %in% group_v){
            cnt_untl = 1
            cur_v <- c(el)
            grp_status <- TRUE
          }else{
            cnt_untl = 0
            rtn_v <- c(rtn_v, el)
          }
        }else{
          rtn_v <- c(rtn_v, el)
        }
      }
      if (length(cur_v) > 0){
        rtn_v <- c(rtn_v, paste(cur_v, collapse = ""))
      }
      return(rtn_v)
    }
    regex_spe_detect <- function(inpt){
          fillr <- function(inpt_v, ptrn_fill="\\.\\.\\.\\d"){
            ptrn <- grep(ptrn_fill, inpt_v)
            while (length(ptrn) > 0){
              ptrn <- grep(ptrn_fill, inpt_v)
              idx <- ptrn[1] 
              untl <- as.numeric(c(unlist(strsplit(inpt_v[idx], split="\\.")))[4]) - 1
              pre_val <- inpt_v[(idx - 1)]
              inpt_v[idx] <- pre_val
              if (untl > 0){
                for (i in 1:untl){
                  inpt_v <- append(inpt_v, pre_val, idx)
                }
              }
            ptrn <- grep(ptrn_fill, inpt_v)
            }
            return(inpt_v)
          }
          inpt <- unlist(strsplit(x=inpt, split=""))
          may_be_v <- c("[", "]", "{", "}", "-", "_", ".", "(", ")", "/", "%", "*", "^", "?", "$")
          pre_idx <- unique(match(x=inpt, table=may_be_v))
          pre_idx <- pre_idx[!(is.na(pre_idx))]
          for (el in may_be_v[pre_idx]){
                  cnt = 0
                  for (i in grep(pattern=paste("\\", el, sep=""), x=inpt)){
                          inpt <- append(x=inpt, values="\\", after=(i - 1 + cnt))
                          cnt = cnt + 1
                  }
          }
            return(paste(inpt, collapse=""))
    }
    for (split in split_v){
      pre_inpt <- inpt
      inpt <- c()
      lst_splt <- FALSE
      for (el in pre_inpt){
        cur_splt <- unlist(strsplit(x = el, split = regex_spe_detect(split)))
        cur_splt[cur_splt == ""] <- split
        cnt = 1
        while (cnt < length(cur_splt)){
          if (cur_splt[cnt] %in% split_v){
            if (lst_splt & cur_splt[cnt] != split){
              cur_splt <- append(x = cur_splt, values = split, after = cnt)
              cnt = cnt + 2 
            }
            lst_splt <- TRUE
            cnt = cnt + 1
          }else{
            cur_splt <- append(x = cur_splt, values = split, after = cnt)
            lst_splt <- FALSE
            cnt = cnt + 2
          }
        }
        cur_grp <- c()
        split_decomp <- unlist(strsplit(x = split, split = ""))
        for (el2 in split_decomp){ cur_grp <- c(cur_grp, el2) }
        last_verif <- glue_groupr_v(unlist(strsplit(x = el, split = "")), group_v = cur_grp, untl = nchar(split))
        if (sum(grepl(x = last_verif, pattern = regex_spe_detect(split))) == (sum(grepl(x = cur_splt, pattern = regex_spe_detect(split))) + 1)){
          cur_splt <- c(cur_splt, split)
        }
        inpt <- c(inpt, cur_splt)
      }
    }
    return(inpt)
  }
  untl <- c(matrix(nrow = (length(to_find_v) + 1), ncol = 1, data = 1))
  to_find_v <- c(to_find_v, to_find_v[length(to_find_v)])
  pairs_idx <- pairs_findr(inpt = inpt, ptrn1 = "(", ptrn2 = ")")
  pairs_pairs <- unlist(pairs_idx[1])
  inpt <- better_split_any(inpt = inpt, split = c("(", ")", ":"))
  pairs_idx <- dynamic_idx_convertr(from_v_ids = unlist(pairs_idx[2]), from_v_val = inpt)
  i = 1
  cur_inpt <- inpt
  stay_inpt <- inpt
  cur_id_v <- c()
  lngth_track = 0
  lngth_track_v <- c(0)
  while (TRUE){
    if (cur_inpt[1] == to_find_v[i] & cur_inpt[2] == "("){
      cur_inpt[1] <- "?"  
    }
    cur_id <- grep(x = cur_inpt, pattern = paste0("^", to_find_v[i], "$"))
    if (length(cur_id) > 0){
      ok_status <- TRUE
      cur_id <- cur_id[untl[i]]
    }else{
      ok_status <- FALSE
    }
    if (ok_status & i <= length(to_find_v)){
      cur_mtch <- match(table = cur_inpt, x = ":")
      if(!(is.na(cur_mtch)) & i == length(to_find_v)){
        rtn_val <- cur_inpt[(cur_mtch + 1)]
        if (rtn_val == ")"){
          return("Value not found")
        }else{
          return(cur_inpt[(cur_mtch + 1)])
        }
      }
      cur_id_v <- c(cur_id_v, cur_id)
      cur_inpt <- inpt[(cur_id + lngth_track):(pairs_idx[grep(pattern = pairs_pairs[match(x = (cur_id + lngth_track_v[length(lngth_track_v)] - 1), table  = pairs_idx)], x = pairs_pairs)[2]] - 1)] 
      i = i + 1
      lngth_track = lngth_track + cur_id - 1
      lngth_track_v <- c(lngth_track_v, lngth_track)
    }else if (i > 1 & length(cur_id) == 0){
      if (i > 1){
        i = i - 1
        untl[i] = untl[i] + 1
      }
      lngth_track_v <- lngth_track_v[1:(length(lngth_track_v) - 1)]
      if (length(lngth_track_v) > 1){
        lngth_track <- lngth_track_v[length(lngth_track_v)]
      }else{
        lngth_track <- 1
      }
      if (length(cur_id_v) > 1){ 
        cur_id_v <- cur_id_v[1:(length(cur_id_v) - 1)] 
        cur_grep <- grep(pattern = pairs_pairs[match(x = (lngth_track), table  = pairs_idx)], x = pairs_pairs)
        cur_inpt <- inpt[(pairs_idx[cur_grep[1]] + 1):(pairs_idx[cur_grep[2]] - 1)] 
      }else{
        cur_inpt <- stay_inpt
      }
      lngth_track <- 0
    }else{
      return("Value not found")
    }
  }
}

#' write_edm_parser
#'
#' Allow to write data to edm parsed dataset, see examples
#'
#' @param inpt is the input dataset
#' @param to_write_v is the vector containing the path to write the data, see examples
#'
#' @examples
#'
#' print(write_edm_parser("(ok(ee:56))(ok(oui(rr((rr2:6)(rr:5))))(oui(bb(rr2:1)))(ee1:4))", 
#' to_write_v = c("ok", "ee"), write_data = c("ii", "olm")))
#'
#' [1] "(ok(ee:56)(ii:olm))(ok(oui(rr((rr2:6)(rr:5))))(oui(bb(rr2:1)))(ee1:4))"
#'
#' print(write_edm_parser("(ok(ee:56))(ok(oui(rr((rr2:6)(rr:5))))(oui(bb(rr2:1)))(ee1:4))", 
#' to_write_v = c("ok", "oui"), write_data = c("ii", "olm")))
#'
#' [1] "(ok(ee:56))(ok(oui(rr((rr2:6)(rr:5))))(ii:olm)(oui(bb(rr2:1)))(ee1:4))"
#' 
#' print(write_edm_parser("(ok(ee:56))(ok(oui(rr((rr2:6)(rr:5))))(oui(bb(rr2:1)))(ee1:4))", 
#' to_write_v = c("ok", "oui", "oui"), write_data = c("ii", "olm")))
#'
#' [1] "(ok(ee:56))(ok(oui(rr((rr2:6)(rr:5))))(oui(bb(rr2:1)))(ii:olm)(ee1:4))"
#'
#' @export

write_edm_parser <- function(inpt, to_write_v, write_data){
  dynamic_idx_convertr <- function(from_v_ids, from_v_val){
    lngth <- length(from_v_ids)
    i = 1
    no_stop <- TRUE
    I = 1
    lst_nchar = 0
    while (I <= length(from_v_val) & no_stop){
      while ((nchar(from_v_val[I]) + lst_nchar) >= from_v_ids[i] & no_stop){
        from_v_ids[i] <- I
        if (i == length(from_v_ids)){ no_stop <- FALSE }else{ i = i + 1 }
      }
      lst_nchar = lst_nchar + nchar(from_v_val[I])
      I = I + 1
    }
    return(from_v_ids)
  }
  pairs_findr <- function(inpt, ptrn1="(", ptrn2=")"){
          
        regex_spe_detect <- function(inpt){
                fillr <- function(inpt_v, ptrn_fill="\\.\\.\\.\\d"){
                  ptrn <- grep(ptrn_fill, inpt_v)
                  while (length(ptrn) > 0){
                    ptrn <- grep(ptrn_fill, inpt_v)
                    idx <- ptrn[1] 
                    untl <- as.numeric(c(unlist(strsplit(inpt_v[idx], split="\\.")))[4]) - 1
                    pre_val <- inpt_v[(idx - 1)]
                    inpt_v[idx] <- pre_val
                    if (untl > 0){
                      for (i in 1:untl){
                        inpt_v <- append(inpt_v, pre_val, idx)
                      }
                    }
                  ptrn <- grep(ptrn_fill, inpt_v)
                  }
                  return(inpt_v)
                }
                inpt <- unlist(strsplit(x=inpt, split=""))
                may_be_v <- c("[", "]", "{", "}", "-", "_", ".", "(", ")", "/", "%", "*", "^", "?", "$")
                pre_idx <- unique(match(x=inpt, table=may_be_v))
                pre_idx <- pre_idx[!(is.na(pre_idx))]
                for (el in may_be_v[pre_idx]){
                        cnt = 0
                        for (i in grep(pattern=paste("\\", el, sep=""), x=inpt)){
                                inpt <- append(x=inpt, values="\\", after=(i - 1 + cnt))
                                cnt = cnt + 1
                        }
                }
                  return(paste(inpt, collapse=""))
          }
  
          lst <- unlist(strsplit(x=inpt, split=""))
  
          lst_par <- c()
  
          lst_par_calc <- c()
  
          lst_pos <- c()
  
          paires = 1
  
          pre_paires = 1
  
          pre_paires2 = 1
  
          if ((length(grep(x=lst, pattern=regex_spe_detect(inpt=ptrn1))) * 2) > 0){
  
                  for (i in 1:(length(grep(x=lst, pattern=regex_spe_detect(inpt=ptrn1))) * 2)){ 
  
                          lst_par <- c(lst_par, 0)
  
                          lst_par_calc <- c(lst_par_calc, 0)
  
                          lst_pos <- c(lst_pos, 0)
  
  
                  }
  
          }
  
          vec_ret <- c()
  
          par_ = 1
  
          lvl_par = 0
  
          for (el in 1:length(lst)){
  
             if (lst[el] == ptrn1){
  
                     if (!(is.null(vec_ret))){
  
                             lst_par_calc[pre_paires2:pre_paires][-vec_ret] <- lst_par_calc[pre_paires2:pre_paires][-vec_ret] + 1
  
                     }else{
  
                             lst_par_calc[pre_paires2:pre_paires] <- lst_par_calc[pre_paires2:pre_paires] + 1
  
                     }
  
                     pre_paires = pre_paires + 1
  
                     pre_cls <- TRUE
  
                     lst_pos[par_] <- el
  
                     par_ = par_ + 1
  
                     lvl_par = lvl_par + 1
  
             }
  
             if (lst[el] == ptrn2){
  
                     lvl_par = lvl_par - 1
  
                     if (!(is.null(vec_ret))){
  
                          lst_par_calc[c(pre_paires2:pre_paires)][-vec_ret] <- lst_par_calc[pre_paires2:pre_paires][-vec_ret] - 1
  
                          pre_val <- lst_par_calc[pre_paires2:pre_paires][vec_ret]
  
                          lst_par_calc[pre_paires2:pre_paires][vec_ret] <- (-2)
                     
                     }else{
  
                          lst_par_calc[c(pre_paires2:pre_paires)] <- lst_par_calc[pre_paires2:pre_paires] - 1
  
                     }
  
                     if (!(is.null(vec_ret))){ 
  
                             pre_mtch <- match(x=c(0, -1), table=lst_par_calc[pre_paires2:pre_paires][-vec_ret])
  
                             lst_par_calc[pre_paires2:pre_paires][vec_ret] <- pre_val 
  
                     }else{
  
                             pre_mtch <- match(x=c(0, -1), table=lst_par_calc[pre_paires2:pre_paires])
  
                     }
  
                     cnt_par = 1
  
                     cnt2 = 0
  
                     if (!(is.null(vec_ret))){
  
                             vec_ret <- sort(vec_ret)
  
                             if (pre_mtch[1] >= min(vec_ret)){
  
                                  cnt2 = 2
  
                                  while (pre_mtch[1] > cnt_par & cnt2 <= length(vec_ret)){
  
                                          if ((vec_ret[cnt2] - vec_ret[(cnt2 - 1)]) > 1){
  
                                                  cnt_par = cnt_par + (vec_ret[cnt2] - vec_ret[(cnt2 - 1)]) - 1
  
                                          }
  
                                          cnt2 = cnt2 + 1
  
                                  }
  
                                  if (pre_mtch[1] > cnt_par){
  
                                          cnt_par = length(vec_ret) / 2 + 1
  
                                  }
  
                                  cnt2 = cnt2 - 1
  
                             }
  
                     }
  
                     lst_par[pre_mtch[1] + (pre_paires2 - 1) + ifelse(cnt2 %% 2 == 0, cnt2, (cnt2 - 1))] <- paires 
  
                     lst_par[pre_mtch[2] + (pre_paires2 - 1) + length(vec_ret)] <- paires 
  
                     if ((pre_mtch[1] + (pre_paires2 - 1)) == 1){
  
                          pre_paires2 = pre_mtch[2] + (pre_paires2 - 1) + length(vec_ret) + 1
  
                          vec_ret <- c()
  
                          cnt_par = 0
  
                     } else if (lst_par_calc[(pre_mtch[1] + (pre_paires2 - 1) - 1)] == -1 & ifelse(is.null(vec_ret), TRUE, 
                                  is.na(match(x=-1, table=lst_par_calc[pre_paires2:pre_paires][-vec_ret])))){
  
                          pre_paires2 = pre_mtch[2] + (pre_paires2 - 1) + length(vec_ret) + 1
  
                          vec_ret <- c()
  
                          cnt_par = 0
  
                     } else{
  
                          vec_ret <- c(vec_ret, (pre_mtch[1]) + ifelse(cnt2 %% 2 == 0, cnt2, (cnt2 - 1)), 
                                       (pre_mtch[2] + length(vec_ret)))
  
                     }
  
                     paires = paires + 1
  
                     pre_paires = pre_paires + 1
  
                     pre_cls <- FALSE
  
                     lst_pos[par_] <- el
  
                     par_ = par_ + 1
  
             }
  
          }
  
          return(list(lst_par, lst_pos))
  
  }
  better_split_any <- function(inpt, split_v = c()){
    glue_groupr_v <- function(inpt_v, group_v = c(), untl){
      rtn_v <- c()
      cur_v <- c()
      grp_status <- FALSE
      cnt_untl = 0
      for (el in inpt_v) {
        if (el %in% group_v & cnt_untl < untl){
          grp_status <- TRUE
          cur_v <- c(cur_v, el)
          cnt_untl = cnt_untl + 1
        }else if (grp_status){
          grp_status <- FALSE
          rtn_v <- c(rtn_v, paste(cur_v, collapse = ""))
          cur_v <- c()
          if (el %in% group_v){
            cnt_untl = 1
            cur_v <- c(el)
            grp_status <- TRUE
          }else{
            cnt_untl = 0
            rtn_v <- c(rtn_v, el)
          }
        }else{
          rtn_v <- c(rtn_v, el)
        }
      }
      if (length(cur_v) > 0){
        rtn_v <- c(rtn_v, paste(cur_v, collapse = ""))
      }
      return(rtn_v)
    }
    regex_spe_detect <- function(inpt){
          fillr <- function(inpt_v, ptrn_fill="\\.\\.\\.\\d"){
            ptrn <- grep(ptrn_fill, inpt_v)
            while (length(ptrn) > 0){
              ptrn <- grep(ptrn_fill, inpt_v)
              idx <- ptrn[1] 
              untl <- as.numeric(c(unlist(strsplit(inpt_v[idx], split="\\.")))[4]) - 1
              pre_val <- inpt_v[(idx - 1)]
              inpt_v[idx] <- pre_val
              if (untl > 0){
                for (i in 1:untl){
                  inpt_v <- append(inpt_v, pre_val, idx)
                }
              }
            ptrn <- grep(ptrn_fill, inpt_v)
            }
            return(inpt_v)
          }
          inpt <- unlist(strsplit(x=inpt, split=""))
          may_be_v <- c("[", "]", "{", "}", "-", "_", ".", "(", ")", "/", "%", "*", "^", "?", "$")
          pre_idx <- unique(match(x=inpt, table=may_be_v))
          pre_idx <- pre_idx[!(is.na(pre_idx))]
          for (el in may_be_v[pre_idx]){
                  cnt = 0
                  for (i in grep(pattern=paste("\\", el, sep=""), x=inpt)){
                          inpt <- append(x=inpt, values="\\", after=(i - 1 + cnt))
                          cnt = cnt + 1
                  }
          }
            return(paste(inpt, collapse=""))
    }
    for (split in split_v){
      pre_inpt <- inpt
      inpt <- c()
      lst_splt <- FALSE
      for (el in pre_inpt){
        cur_splt <- unlist(strsplit(x = el, split = regex_spe_detect(split)))
        cur_splt[cur_splt == ""] <- split
        cnt = 1
        while (cnt < length(cur_splt)){
          if (cur_splt[cnt] %in% split_v){
            if (lst_splt & cur_splt[cnt] != split){
              cur_splt <- append(x = cur_splt, values = split, after = cnt)
              cnt = cnt + 2 
            }
            lst_splt <- TRUE
            cnt = cnt + 1
          }else{
            cur_splt <- append(x = cur_splt, values = split, after = cnt)
            lst_splt <- FALSE
            cnt = cnt + 2
          }
        }
        cur_grp <- c()
        split_decomp <- unlist(strsplit(x = split, split = ""))
        for (el2 in split_decomp){ cur_grp <- c(cur_grp, el2) }
        last_verif <- glue_groupr_v(unlist(strsplit(x = el, split = "")), group_v = cur_grp, untl = nchar(split))
        if (sum(grepl(x = last_verif, pattern = regex_spe_detect(split))) == (sum(grepl(x = cur_splt, pattern = regex_spe_detect(split))) + 1)){
          cur_splt <- c(cur_splt, split)
        }
        inpt <- c(inpt, cur_splt)
      }
    }
    return(inpt)
  }
  untl <- c(matrix(nrow = length(to_write_v), ncol = 1, data = 1))
  #to_write_v <- c(to_write_v, to_write_v[length(to_write_v)])
  pairs_idx <- pairs_findr(inpt = inpt, ptrn1 = "(", ptrn2 = ")")
  pairs_pairs <- unlist(pairs_idx[1])
  inpt <- better_split_any(inpt = inpt, split = c("(", ")", ":"))
  pairs_idx <- dynamic_idx_convertr(from_v_ids = unlist(pairs_idx[2]), from_v_val = inpt)
  i = 1
  cur_inpt <- inpt
  stay_inpt <- inpt
  cur_id_v <- c()
  lngth_track = 0
  lngth_track_v <- c(0)
  while (i <= length(to_write_v)){
    if (cur_inpt[1] == to_write_v[i] & cur_inpt[2] == "("){
      cur_inpt[1] <- "?"  
    }
    cur_id <- grep(x = cur_inpt, pattern = paste0("^", to_write_v[i], "$"))
    if (length(cur_id) > 0){
      ok_status <- TRUE
      cur_id <- cur_id[untl[i]]
    }else{
      ok_status <- FALSE
    }
    if (ok_status & i <= length(to_write_v)){
      cur_id_v <- c(cur_id_v, cur_id)
      cur_inpt <- inpt[(cur_id + lngth_track):(pairs_idx[grep(pattern = pairs_pairs[match(x = (cur_id + lngth_track_v[length(lngth_track_v)] - 1), table  = pairs_idx)], x = pairs_pairs)[2]] - 1)] 
      i = i + 1
      lngth_track = lngth_track + cur_id - 1
      lngth_track_v <- c(lngth_track_v, lngth_track)
    }else if (i > 1 & length(cur_id) == 0){
      if (i > 1){
        i = i - 1
        untl[i] = untl[i] + 1
      }
      lngth_track_v <- lngth_track_v[1:(length(lngth_track_v) - 1)]
      if (length(lngth_track_v) > 1){
        lngth_track <- lngth_track_v[length(lngth_track_v)]
      }else{
        lngth_track <- 1
      }
      if (length(cur_id_v) > 1){ 
        cur_id_v <- cur_id_v[1:(length(cur_id_v) - 1)] 
        cur_grep <- grep(pattern = pairs_pairs[match(x = (lngth_track), table  = pairs_idx)], x = pairs_pairs)
        cur_inpt <- inpt[(pairs_idx[cur_grep[1]] + 1):(pairs_idx[cur_grep[2]] - 1)] 
      }else{
        cur_inpt <- stay_inpt
      }
      lngth_track <- 0
    }else{
      return("Value not found")
    }
  }
  inpt <- append(x = inpt, values = paste0(c("(", write_data[1], ":", write_data[2], ")"), collapse = ""), after = pairs_idx[grep(pattern = pairs_pairs[match(x = lngth_track_v[length(lngth_track_v)], table = pairs_idx)], x = pairs_pairs)[2]])
  return(paste(inpt, collapse = ""))
}




