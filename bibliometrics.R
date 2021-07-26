# remotes::install_github("njahn82/semscholar")

library("tidyverse")
library("semscholar")

#-----------------------------------------------------------------------------
# Get Semantic Scholar metadata for Lewontin 1972 and all citing papers
#-----------------------------------------------------------------------------

l1972 <- s2_papers(
  c("10.1007/978-1-4684-9063-3_14"))

l1972_citations <- l1972$citations[[1]]

# cite_doi_list <- l1972_citations %>% 
#   dplyr::filter(!is.na(citations_doi)) %>% 
#   dplyr::filter(!grepl("\\[|<", citations_doi)) %>% 
#   pull(citations_doi)

cite_doi_list <- l1972_citations$citations_paper_id

citing_data <- list()

# wrap API call in possibly() to catch errors
s2_papers_safe <- possibly(s2_papers, otherwise=tibble())

if(file.exists("data/papers_citing_lewontin_1972.rds")){
  citing_data_df <- readRDS("data/papers_citing_lewontin_1972.rds")
} else {
  for(i in 1:length(unique(cite_doi_list))){
    citing_data[[i]] <- s2_papers_safe(cite_doi_list[i])
    Sys.sleep(3.1) # add delay to avoid API rate limits
  }
  
  # drop two papers with malformed data
  citing_data[[1788]] <- NULL
  citing_data[[1789]] <- NULL
  
  # additional influential citing papers IDed from GS that were missing from SS
  nei_1973 <- s2_papers_safe("e00ae5c3741f5d9e96a7b460d5a3809f13cabd93")
  rosenberg_2002 <- s2_papers_safe("5f2cc2f6287b76d64b0b7fc922d5de24c75cff79")
  bowcock_1994 <- s2_papers_safe("9bcdc494ead988e2ebc49b7f074c72e765fd15a7")
  kurzban_2001 <- s2_papers_safe("269a312f68cb3987808834b38ba48f2ca0632be7")
  
  citing_data_df <- bind_rows(citing_data) %>%
    bind_rows(nei_1973) %>%
    bind_rows(rosenberg_2002) %>%
    bind_rows(bowcock_1994) %>%
    bind_rows(kurzban_2001) %>%
    rowwise() %>%
    mutate(ncites = nrow(citations))
  
  saveRDS(citing_data_df, "data/papers_citing_lewontin_1972.rds")
}


#-----------------------------------------------------------------------------
# Fig 1a: cumulative citations
#-----------------------------------------------------------------------------
citing_data_df %>% 
  group_by(year) %>% 
  count() %>% 
  arrange(year) %>% 
  ungroup() %>% 
  mutate(cum_tot=cumsum(n)) %>%
  mutate(tot=sum(n)) %>%
  mutate(prop=cum_tot/tot) %>%
  ggplot(aes(x=year, y=prop))+
  geom_point()+
  geom_line()+
  ylab("Cumulative fraction of citations received")+
  xlab("Year")+
  theme_bw()

#-----------------------------------------------------------------------------
# Fig 1b: citation distributions
#-----------------------------------------------------------------------------
ggplot(l1972_citations, aes(x=citations_year))+
  geom_histogram(binwidth=1)+
  xlab("Year")+
  ylab("Number of citing papers published")+
  theme_bw() #geom_vline(xintercept=1990)+geom_vline(xintercept=2001)

ggplot(l1972_citations, aes(x=citations_year))+
  geom_histogram(binwidth=1)+geom_vline(xintercept=1990)+geom_vline(xintercept=2001)

ggplot(cites_df7, aes(x=year, fill=field))+
  geom_histogram(binwidth=1)+geom_vline(xintercept=1990)+geom_vline(xintercept=2001)

#-----------------------------------------------------------------------------
# Fig 1c: Total citations + influential citations received by citing papers
#-----------------------------------------------------------------------------
cddf_inf_cite <- citing_data_df %>%
  mutate(influential_cite = ifelse(ncites>1000, paste0(title, " (", year, ")"), "all other publications")) %>%
  group_by(year, influential_cite) %>%
  summarise(total_cites=sum(ncites))

# relevel by date
levs <- c(cddf_inf_cite %>% 
            dplyr::filter(influential_cite != "all other publications") %>% 
            pull(influential_cite),
          "all other publications")

cddf_inf_cite %>%
  ungroup() %>%
  mutate(influential_cite = factor(influential_cite, levels=levs)) %>%
  ggplot(aes(x=year, y=total_cites, fill=influential_cite))+
  geom_col()+
  # geom_vline(xintercept=1990)+
  # geom_vline(xintercept=2001)+
  scale_x_continuous(expand=c(0,0), breaks=seq(1970,2020, by=5))+
  scale_fill_manual(name="", values = c(brewer.pal(8, name = "Dark2"), "red", "blue", "purple", "grey80"))+
  ylab("total citations received by \n papers published in a given year \n that cite Lewontin, 1972")+
  theme_classic()+
  theme()

# citing_data_df %>%
#   mutate(influential_cite = ifelse(ncites>500, "papers with >1000 citations", "papers with <1000 citations")) %>%
#   group_by(year, influential_cite) %>%
#   summarise(total_cites=sum(influential_citation_count)) %>%
# ggplot(aes(x=year, y=total_cites, fill=influential_cite))+
#   geom_col()+
#   # geom_vline(xintercept=1990)+
#   # geom_vline(xintercept=2001)+
#   ylab("total influential citations received \n by citing papers published in a given year that cite Lewontin, 1972")+
#   theme_classic()+
#   theme()

#-----------------------------------------------------------------------------
# Load Scopus journal taxonomy data 
#-----------------------------------------------------------------------------
# Retrieved from https://www.elsevier.com/?a=91122
# The first sheet in this Excel file  was converted to .csv format and archived here in the data directory
scopus_data <- read_csv("data/ext_list_October_2020.csv")

scopus_issn_mapping <- scopus_data %>% 
  dplyr::select(print_issn=`Print-ISSN`, 
                e_issn=`E-ISSN`, 
                `Top level:\n\nLife Sciences`:`Top level:\n\nHealth Sciences`)

scopus_issn_long <- scopus_issn_mapping %>% pivot_longer(3:6)

scopus_issn_long2 <- scopus_issn_long %>% dplyr::filter(!is.na(value))

scopus_issn_long3 <- scopus_issn_long2 %>%
  dplyr::select(print_issn, e_issn, field=value) %>%
  pivot_longer(1:2, names_to="type", values_to="issn")

scopus_issn_long4 <- scopus_issn_long3 %>% dplyr::filter(!is.na(issn))

#-----------------------------------------------------------------------------
# Get citation data from OpenCitations (contains ISSNs needed to identify field)
#-----------------------------------------------------------------------------

# function to fill in extra metadata from CrossRef
get_cites <- function (doi) {
  return(tryCatch(rcrossref::cr_works(doi)$data, error=function(e) NULL))
}

if(file.exists("data/opcit_papers_citing_lewontin_1972.rds")){
  cites_df <- readRDS("data/opcit_papers_citing_lewontin_1972.rds")
} else {
  opcit <- "https://opencitations.net/index/coci/api/v1/citations/10.1007/978-1-4684-9063-3_14"
  
  citations_df <- jsonlite::fromJSON(opcit) %>% as.data.frame
  
  citations_df2 <- rcrossref::cr_works(citations_df$citing)
  
  cites <- lapply(l1972_citations$citations_doi, get_cites)
  cites_df <- cites %>% purrr::reduce(bind_rows)
  
  saveRDS(cites_df, "data/opcit_papers_citing_lewontin_1972.rds")
}

cites_df_trimmed <- cites_df %>%
  dplyr::select(container.title, created, issued, doi, issn)

# separate the two ISSNs
cites_df2 <- cites_df_trimmed %>% 
  mutate(issn_new=gsub("-", "", issn)) %>% 
  separate(issn_new, c("issn1", "issn2"))

# merge with Scopus data
cites_df3 <- inner_join(cites_df2, scopus_issn_long4, by=c("issn1" = "issn"))
cites_df4 <- inner_join(cites_df2, scopus_issn_long4, by=c("issn2" = "issn"))
cites_df5 <- bind_rows(cites_df3, cites_df4)
cites_df6 <- pivot_wider(cites_df5, id_cols=c(doi, field, issued))

#-----------------------------------------------------------------------------
# Fig 2: Plot citation distributions by field
#-----------------------------------------------------------------------------
cites_by_field <- cites_df6 %>%
  group_by(doi) %>%
  slice(1L) %>% 
  #left_join(l1972_citations, by=c("doi" = "citations_doi")) %>%
  mutate(doi=tolower(doi)) %>%
  # inner_join(l1972_citations %>% mutate(doi=tolower(citations_doi))) %>% 
  inner_join(citing_data_df %>% mutate(doi=tolower(doi))) %>% 
  mutate(year=as.numeric(gsub("-.*", "", issued))) %>%
  mutate(year5=floor(as.numeric(year)/5)*5)

cites_by_field_count <- cites_by_field %>%
  group_by(year, field) %>%
  #summarise(n = sum(ncites)) %>% #head
  count(drop=FALSE) %>%
  group_by(year) %>%
  mutate(tot=sum(n)) %>%
  rowwise() %>%
  mutate(prop=n/tot) %>% 
  ungroup()

# Fig 2a
cites_by_field_count %>%
  ggplot(aes(x=year, y=n, fill=field))+
  geom_bar(stat="identity")+
  facet_wrap(~field)+
  scale_x_continuous(expand=c(0,0), breaks=seq(1970,2020, by=5))+
  ylab("Number of citing papers published")+
  theme_classic()+
  theme(legend.position="none")

# Fig 2b
cites_by_field %>%
  group_by(year5, field) %>%
  #summarise(n = sum(ncites)) %>% #head
  count(drop=FALSE) %>%
  group_by(year5) %>%
  mutate(tot=sum(n)) %>%
  rowwise() %>%
  mutate(prop=n/tot) %>% 
  ungroup() %>%
  ggplot(aes(x=year5, y=prop, fill=field))+
  geom_col()+
  scale_x_continuous(expand=c(0,0),
                     breaks=seq(1970,2020, by=5), 
                     labels=paste(seq(1970, 2020, 5), seq(1975, 2025, 5), sep="-"))+
  ylab("Fraction of citations in 5-year period")+
  xlab("Year")+
  theme_classic()

# cites_by_field %>%
#   rowwise() %>%
#   #mutate(ncites=nrow(citations)) %>%
#   ggplot(aes(log(ncites), colour=field))+
#   geom_density()

#-----------------------------------------------------------------------------
# Co-citation analysis
#-----------------------------------------------------------------------------
citing_ref_df <- bind_rows(citing_data_df$references, .id = "column_label") %>% 
  mutate(column_label=as.numeric(column_label))

citing_data_df2 <- citing_data_df %>% 
  ungroup() %>% 
  mutate(column_label=row_number())

citing_data_df_ref <- left_join(citing_data_df2, citing_ref_df)

top_co_cites <- citing_data_df_ref %>% 
  group_by(year, references_title, references_year, references_doi) %>% 
  count() %>% 
  group_by(year) %>% 
  mutate(tot_refs=sum(n)) %>% 
  rowwise() %>% 
  mutate(prop=n/tot_refs) %>%
  ungroup() %>%
  #dplyr::filter(n>1) %>% 
  arrange(year, desc(n)) %>% 
  dplyr::filter(references_title != "The Apportionment of Human Diversity") %>% 
  group_by(year) %>% 
  slice_head(n=5)

top_co_cites_overall <- citing_data_df_ref %>% 
  group_by(references_title, references_year, references_doi) %>% 
  count() %>% 
  # group_by(year) %>% 
  mutate(tot_refs=sum(n)) %>% 
  rowwise() %>% 
  mutate(prop=n/tot_refs) %>%
  ungroup() %>%
  #dplyr::filter(n>1) %>% 
  arrange(desc(n)) %>% 
  dplyr::filter(references_title != "The Apportionment of Human Diversity") #%>% 
  # group_by(year) %>% 
  # slice_head(n=5)

selected_co_refs <- table(top_co_cites$references_title) %>% data.frame %>% dplyr::filter(Freq>2)

#-----------------------------------------------------------------------------
# Fig 3a: top co-citations
#-----------------------------------------------------------------------------
top_co_cites %>% 
  dplyr::filter(references_title %in% selected_co_refs$Var1) %>%
  dplyr::filter(year>=1995) %>%
  ggplot(aes(x=year, y=n, colour=paste0(references_title, " (", references_year, ")")))+
  geom_point()+
  geom_line()+
  scale_colour_discrete(name="Top co-cited papers")+
  scale_x_continuous(limits=c(1995,2021), breaks=seq(1995,2020, by=5), labels=seq(1995,2020, by=5))+
  ylab("Number of papers in which \n Lewontin, 1972 is co-cited")+
  theme_classic()+
  theme()

#-----------------------------------------------------------------------------
# Fig 3b: plot citation trajectories of comparable papers
#-----------------------------------------------------------------------------

# Nei, 1972
n1972 <- s2_papers("cdc0ee01533e280cd5c0f3ee392be6daed59689a")
n1972_citations <- n1972$citations[[1]]

# Nei, 1973
n1973 <- s2_papers("10.1073/pnas.70.12.3321")
n1973_citations <- n1973$citations[[1]]

# Nei, 1978
n1978 <- s2_papers("81f8fb4361b2bfa0159d5165641b87d30a9701f6")
n1978_citations <- n1978$citations[[1]]

# Kimura & Crow, 1964
k1964 <- s2_papers("c5d40144278d71b88b75f9682e979181b7867fca")
k1964_citations <- k1964$citations[[1]]

nr1972 <- s2_papers("c2d3394b9a5ab1753f70fba67e3d99082a3dad7e")
nr1972_citations <- nr1972$citations[[1]]

nr1974 <- s2_papers("b275116ce7edcca74194d2acb88d90247d9ba53b")
nr1974_citations <- nr1974$citations[[1]]

bind_rows(l1972_citations, 
          n1972_citations, 
          n1973_citations, 
          n1978_citations, 
          k1964_citations,
          nr1972_citations,
          nr1974_citations, .id="paper") %>%
  mutate(paper=recode(paper,
                      "1" = "Lewontin, 1972",
                      "2" = "Nei, 1972",
                      "3" = "Nei, 1973",
                      "4" = "Nei, 1978",
                      "5" = "Kimura & Crow, 1964", #)) %>%
                      "6" = "Nei & Roychoudhury, 1972",
                      "7" = "Nei & Roychoudhury, 1974")) %>%
  group_by(citations_year, paper) %>%
  count() %>%
  group_by(paper) %>%
  mutate(tot = sum(n)) %>%
  rowwise() %>%
  mutate(norm_cite_rate = n/tot) %>%
  ungroup() %>%
  ggplot(aes(x=citations_year, y=norm_cite_rate, colour=paper, shape=paper))+
  geom_point()+
  geom_line()+
  ylab("fraction of total citations per paper")+
  xlab("Year")+
  theme_classic()+
  theme()

#-----------------------------------------------------------------------------
# examine top co-citations among articles citing Edwards, 2003
#-----------------------------------------------------------------------------

e2003 <- s2_papers("2f6295c900dd40377e60b6ca07ed1dc155c71504")
e2003_citations <- e2003$citations[[1]]

e2003_ref_ids <- e2003_citations$citations_paper_id

e2003_citing_data <- list()

if(file.exists("data/papers_citing_edwards_2003.rds")){
  e2003_citing_data_df <- readRDS("data/papers_citing_edwards_2003.rds")
} else {
  for(i in 1:length(unique(e2003_ref_ids))){
    e2003_citing_data[[i]] <- s2_papers_safe(e2003_ref_ids[i])
    Sys.sleep(3.1) # add delay to avoid API rate limits
  }
  
  e2003_citing_data[[222]] <- NULL
  
  e2003_citing_data_df <- bind_rows(e2003_citing_data) %>%
    rowwise() %>%
    mutate(ncites = nrow(citations))
  
  saveRDS(e2003_citing_data_df, "data/papers_citing_edwards_2003.rds")
}

e2003_citing_ref_df <- bind_rows(e2003_citing_data_df$references, .id = "column_label") %>% 
  mutate(column_label=as.numeric(column_label))

e2003_citing_data_df2 <- e2003_citing_data_df %>% 
  ungroup() %>% 
  mutate(column_label=row_number())

e2003_citing_data_df_ref <- left_join(e2003_citing_data_df2, e2003_citing_ref_df)

e2003_top_co_cites <- e2003_citing_data_df_ref %>% 
  group_by(references_title, references_year, references_doi) %>% 
  count() %>% 
  #group_by(year) %>% 
  mutate(tot_refs=sum(n)) %>% 
  rowwise() %>% 
  mutate(prop=n/tot_refs) %>%
  ungroup() %>%
  #dplyr::filter(n>1) %>% 
  arrange(desc(n)) %>% 
  dplyr::filter(references_title != "Human genetic diversity: Lewontin's fallacy.") #%>% 
  #group_by(year) %>% 
  #slice_head(n=5)

#-----------------------------------------------------------------------------
# Self-citation analysis
#-----------------------------------------------------------------------------

lewontin_auth <- s2_authors(c("3758430"))

if(file.exists("data/self_cites_lewontin_1972.rds")){
  lewontin_self_cites_df <- readRDS("data/self_cites_lewontin_1972.rds")
} else {
  lewontin_paper_ids <- lewontin_auth$papers[[1]]$paper_id
  
  lewontin_refs <- list()
  
  for(i in 1:length(lewontin_paper_ids)){
    lewontin_refs[[i]] <- s2_papers(lewontin_paper_ids[i])
    Sys.sleep(3.1) # make sure we don't hit API rate limits
  }
  
  # remove paper that isn't actually authored by Lewontin
  lewontin_refs[[66]] <- NULL
  
  lewontin_self_cites_df <- bind_rows(lewontin_refs, .id = "column_label") %>%
    rowwise() %>%
    mutate(ncites = nrow(citations))
  
  saveRDS(lewontin_self_cites_df, "data/self_cites_lewontin_1972.rds")
}

# check Lewontin is author
lewontin_self_cites_df %>% dplyr::filter(any(str_detect(unlist(authors), "Lewontin"))) %>% nrow

# check number of Lewontin's pubs where refs are available
lewontin_self_cites_df %>% dplyr::filter(nrow(references)>0) %>% nrow

self_cites <- bind_rows(lewontin_self_cites_df$references, .id = "column_label") %>% 
  rowwise() %>%
  dplyr::filter(any(str_detect(tolower(unlist(references_authors)), "lewontin")))

self_cites_anno <- self_cites %>%
  #mutate(title=tolower(references_title)) %>%
  # mutate(title=substr(tolower(references_title), 1, 30)) %>%
  mutate(title=references_title) %>%
  group_by(title) %>% 
  count %>% 
  arrange(desc(n)) %>%
  ungroup() %>%
  left_join(lewontin_self_cites_df, by = "title") %>%
  mutate(nyears=2021-year) %>%
  mutate(self_citation_rate=n/nyears) %>%
  mutate(paper=ifelse(grepl("Apportionment", title), "Lewontin 1972", "other"))

self_cites_anno %>%
  mutate(era=ifelse(year<1980 & year>=1960, "1960-1979", "1980-present")) %>%
  rowwise() %>%
  mutate(nauthors=nrow(authors)) %>%
  ggplot(aes(x=self_citation_rate, y=log10(ncites)))+
  geom_point(aes(colour=paper, shape=era, size=nauthors), alpha=0.5)+
  geom_smooth(method="lm")
