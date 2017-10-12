compIntestazione = function (ordi, bene){
  o = strsplit(ordi, " ")
  o = matrix(unlist(o), byrow = TRUE)
  b = strsplit(bene, " ")
  b = matrix(unlist(b), byrow = TRUE)
  if(length(o)>length(b)){
    o = b
    bene = ordi
  }
  if(length(o)==2){
    if(grepl(o[1], bene) && grepl(o[2], bene)){
      return (TRUE)
    }
  } else if(length(o)==3){
    if((grepl(paste("\\b",o[1],"\\b", sep=""), bene) && grepl(paste("\\b",o[2],"\\b", sep=""), bene) && grepl(paste("\\b",o[3],"\\b", sep=""), bene))){
      return (TRUE)
    }
  } else {
    
    if(length(o)>3 && ((grepl(paste("\\b",o[1],"\\b", sep=""), bene) && grepl(paste("\\b",o[2],"\\b", sep=""), bene)) || (grepl(paste("\\b",o[3],"\\b", sep=""), bene) && grepl(paste("\\b",o[4],"\\b", sep=""), bene)))){
      return (TRUE)
    }
    else
      return (FALSE)
  }
}

getMatrices = function(months){
  matrix_numero <- data.frame(matrix(nrow=0, ncol=2))
  matrix_importo <- data.frame(matrix(nrow=0, ncol=2))
  names(matrix_numero) <- c("IBAN", months)
  names(matrix_importo) <- c("IBAN", months)
  dat <- data.frame()
  query_tot_bonifici_sql <- paste("select cl.c_ndg, cl.c_forma_giuridica, bon.c_conto_operazione, bon.c_filiale_conto_operazione, 
                                  replace(bon.i_importo, ',', '.') as i_importo, to_char(to_date(bon.d_esecuzione), 'dd-mm-yy') as d_esecuzione, 
                                  bon.c_verso, bon.c_banca, bon.x_causale, 
                                  upper(REGEXP_REPLACE(bon.s_intestazione_beneficiario,'[^a-zA-Z'']',' ')) as s_intestazione_beneficiario,
                                  upper(REGEXP_REPLACE(bon.s_intestazione_ordinante,'[^a-zA-Z'']',' ')) as ordinante, bon.l_iban_ordinante, l_iban_beneficiario
                                  from edb.bonifico bon 
                                  join edb.cliente cl on
                                  bon.c_ndg = cl.c_ndg and bon.c_banca = cl.c_banca
                                  where cl.c_forma_giuridica in ('PF', 'CO')
                                  and bon.c_banca = '05387' and bon.c_verso = 'P'
                                  and bon.s_tipo_destinazione = 'ESTERNO'
                                  and bon.s_stato not in ('RIFIUTATO','ANNULLATO')
                                  and bon.d_esecuzione between TO_DATE('",months,"', 'MON-YYYY') AND LAST_DAY(TO_DATE('",months,"', 'MON-YYYY'))",sep = "")
  query_tot_bonifici <-  dbGetQuery(con, query_tot_bonifici_sql)
  n=0
  tmp1=nrow(query_tot_bonifici)
  t1=Sys.time()
  for (i in 1:tmp1){
    if (isTRUE(compIntestazione(query_tot_bonifici[i,]$ORDINANTE,query_tot_bonifici[i,]$S_INTESTAZIONE_BENEFICIARIO))){
      n=n+1
      dat=rbind(dat,query_tot_bonifici[i,])
      #rw=which(matrix_numero$IBAN == query_tot_bonifici[i,]$L_IBAN_ORDINANTE)
      if(!(query_tot_bonifici[i,]$L_IBAN_ORDINANTE %in% matrix_numero$IBAN)){
        matrix_numero[(nrow(matrix_numero)+1),1]=query_tot_bonifici[i,]$L_IBAN_ORDINANTE
        matrix_numero[nrow(matrix_numero), 2] = 1
        matrix_numero[nrow(matrix_numero), is.na(matrix_numero[nrow(matrix_numero),])] <- 0
        matrix_importo[(nrow(matrix_importo)+1),1]=query_tot_bonifici[i,]$L_IBAN_ORDINANTE
        matrix_importo[nrow(matrix_importo), 2] = as.numeric(query_tot_bonifici[i,]$I_IMPORTO)
        matrix_importo[nrow(matrix_importo), is.na(matrix_importo[nrow(matrix_importo),])] <- 0
      }
      else{
        rw=which(matrix_numero$IBAN == query_tot_bonifici[i,]$L_IBAN_ORDINANTE)
        matrix_numero[rw,2]=matrix_numero[rw,(2)]+1
        matrix_importo[rw,2]=matrix_importo[rw,(2)]+as.numeric(query_tot_bonifici[i,]$I_IMPORTO)
      }    
    }
  }
  return (list(matrix_importo,matrix_numero,dat))
}

