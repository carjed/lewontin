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
  
  avise <- s2_papers_safe("4198c443f02da49348b5c842b4a50ba7ba1cb882")
  gould <- s2_papers_safe("84812fa557d6ee1a926ff8681be9e0900fc097a2")
  wilson <- s2_papers_safe("eba971ec9614672cfa3c907beb78b776aa08a832")
  berry <- s2_papers_safe("3fd63f33b86d22969a307d1b88549afff345155e")
  ewens <- s2_papers_safe("de57bf6d568218b80f2440eb1443d03b8c95dded")
  sarasvathy <- s2_papers_safe("998e91404c92145dd65c2ae1c2a81cc4eca23531")
  l1974 <- s2_papers_safe("59b684b865b27658b8784fc1b0caf802fc119969")
  cs <- s2_papers_safe("0cd61ae6f625a7dd3a5238bc8606e96b87c7afcb")
  
  citing_data_df <- bind_rows(citing_data) %>%
    bind_rows(nei_1973) %>%
    bind_rows(rosenberg_2002) %>%
    bind_rows(bowcock_1994) %>%
    bind_rows(kurzban_2001) %>%
    bind_rows(c(avise, gould, wilson, berry, ewens, sarasvathy, l1974, cs)) %>%
    rowwise() %>%
    mutate(ncites = nrow(citations))
  
  saveRDS(citing_data_df, "data/papers_citing_lewontin_1972.rds")
}


#-----------------------------------------------------------------------------
# Fig 1a: cumulative citations
#-----------------------------------------------------------------------------
fig1a <- citing_data_df %>% 
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
  ylab("Cumulative fraction of\n citations received")+
  ggtitle("a")+
  scale_x_continuous(breaks=seq(1970,2020,5), labels=seq(1970,2020,5))+
  theme_classic()+
  theme(
    legend.title=element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size=12, angle=45, hjust=1),
    axis.text.y = element_text(size=12),
    axis.title.y = element_text(size=16))+
  NULL

#-----------------------------------------------------------------------------
# Fig 1b: citation distributions
#-----------------------------------------------------------------------------
fig1b <- ggplot(l1972_citations, aes(x=citations_year))+
  geom_histogram(binwidth=1)+
  scale_x_continuous(breaks=seq(1970,2020,5), labels=seq(1970,2020,5))+
  # xlab("Year")+
  ylab("Number of citing\n papers published")+
  ggtitle("b")+
  theme_classic()+
  theme(
        legend.title=element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size=12, angle=45, hjust=1),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=16))+
  NULL
  #geom_vline(xintercept=1990)+geom_vline(xintercept=2001)

# ggplot(l1972_citations, aes(x=citations_year))+
#   geom_histogram(binwidth=1)+geom_vline(xintercept=1990)+geom_vline(xintercept=2001)
# 
# ggplot(cites_df7, aes(x=year, fill=field))+
#   geom_histogram(binwidth=1)+geom_vline(xintercept=1990)+geom_vline(xintercept=2001)

#-----------------------------------------------------------------------------
# Fig 1c: Total citations + influential citations received by citing papers
#-----------------------------------------------------------------------------
cddf_inf_cite <- citing_data_df %>%
  rowwise() %>% 
  mutate(nauth=nrow(authors)) %>% 
  rowwise() %>% 
  mutate(auth=authors$author_name[1]) %>% 
  mutate(auth_final=ifelse(nauth==1, auth, paste0(auth, " et al."))) %>% 
  mutate(auth_final=gsub("[A-Z][.] ", "", auth_final)) %>%
  mutate(title=str_to_title(title)) %>%
  # mutate(influential_cite = ifelse(ncites>1000, paste0(auth_final, " (", year, ") ", title), "all other publications")) %>%
  mutate(influential_cite = ifelse(ncites>1000, paste0(auth_final, " (", year, ") "), "all other publications")) %>%
  group_by(year, influential_cite) %>%
  summarise(total_cites=sum(ncites)) %>%
  # fix incorrectly attributed citations
  mutate(influential_cite=gsub("Barker et al.", "Wilson", influential_cite)) %>%
  mutate(influential_cite=gsub("Birx et al.", "Gould", influential_cite)) %>%
  mutate(influential_cite=gsub("Calyampudi ", "", influential_cite)) %>%
  mutate(influential_cite=gsub("David ", "", influential_cite)) %>%
  mutate(influential_cite=gsub("Roberts", "Cavalli-Sforza", influential_cite))

# relevel by date
levs <- c(cddf_inf_cite %>% 
            dplyr::filter(influential_cite != "all other publications") %>% 
            pull(influential_cite),
          "all other publications")

fig1c <- cddf_inf_cite %>%
  ungroup() %>%
  mutate(influential_cite = factor(influential_cite, levels=levs)) %>%
  ggplot(aes(x=year, y=total_cites, fill=influential_cite))+
  geom_col()+
  # geom_vline(xintercept=1990)+
  # geom_vline(xintercept=2001)+
  scale_x_continuous(expand=c(0,0), breaks=seq(1970,2020, by=5))+
  scale_fill_manual(name="", values = c(brewer.pal(9, name = "Reds")[3:9],
                                        brewer.pal(9, name = "Purples")[4:9],
                                        brewer.pal(9, name = "Blues")[4:9], "grey80"))+
  ylab("total citations received by \n papers published in a given year \n that cite Lewontin, 1972")+
  ggtitle("c")+
  guides(fill = guide_legend(ncol = 2))+
  theme_classic()+
  theme()+
  theme(
        legend.title=element_blank(),
        legend.text = element_text(size=12),
        legend.position = "right",
        axis.title.x = element_blank(),
        axis.text.x = element_text(size=12, angle=45, hjust=1),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=16))+
  NULL


grid.arrange(
  grobs = list(fig1a, fig1b, fig1c),
  widths = c(1, 1),
  heights = c(2,3),
  layout_matrix = rbind(c(1, 2),
                        c(3, 3))
)

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
fig2a <- cites_by_field_count %>%
  ggplot(aes(x=year, y=n, fill=field))+
  geom_bar(stat="identity")+
  facet_wrap(~field, ncol=1)+
  scale_fill_brewer(palette="Dark2")+
  scale_x_continuous(expand=c(0,0), breaks=seq(1970,2020, by=5))+
  ylab("Number of citing papers published")+
  xlab("")+
  labs(fill = "Journal Topic")+
  ggtitle("a")+
  theme_classic()+
  theme(
    axis.title = element_text(size=16),
    legend.position="none",
    strip.text = element_text(size=16),
    axis.text.y = element_text(size=12),
    axis.text.x = element_text(size=12, angle=45, hjust=1),
    legend.text = element_text(size=12))+
  NULL

# Fig 2b
fig2b <- cites_by_field %>%
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
  scale_fill_brewer(palette="Dark2")+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0),
                     breaks=seq(1970,2020, by=5), 
                     labels=paste(seq(1970, 2020, 5), seq(1974, 2025, 5), sep="-"))+
  ylab("Fraction of citations in 5-year period")+
  xlab("")+
  labs(fill = "Journal Topic")+
  ggtitle("b")+
  theme_classic()+
  theme(
    axis.title = element_text(size=16),
    legend.title = element_text(size=16),
    legend.position = "none",
    axis.text.y = element_text(size=12),
    axis.text.x = element_text(size=12, angle=45, hjust=1),
    legend.text = element_text(size=12))+
  NULL

ggarrange(fig2a, fig2b, ncol=2)

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
  # mutate(references_year=
  #          ifelse(grepl("POPGENE", references_title), 
  #                 1997, references_year)) %>%
  #   mutate(references_title=
  #          ifelse(grepl("POPGENE", references_title), 
  #                 "PopGene, the user-friendly shareware for population genetic analysis, molecular biology and biotechnology center", 
  #                 references_title)) %>%
  #dplyr::filter(references_year<2021) %>%
  group_by(year, references_authors, references_title, references_year, references_doi) %>% 
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
  slice_head(n=10) %>%
  rowwise() %>% 
  mutate(nauth=nrow(references_authors)) %>% 
  rowwise() %>% 
  mutate(auth=references_authors$name[1]) %>% 
  mutate(auth_final=ifelse(nauth==1, auth, paste0(auth, " et al."))) %>% 
  mutate(auth_final=gsub("[A-Z][.] ", "", auth_final)) %>%
  mutate(title_str = paste0(auth_final, " (", year, ") ", references_title))

top_co_cites_overall <- citing_data_df_ref %>% 
  group_by(references_title, references_authors, references_year, references_doi) %>% 
  count() %>% 
  # group_by(year) %>% 
  mutate(tot_refs=sum(n)) %>% 
  rowwise() %>% 
  mutate(prop=n/tot_refs) %>%
  ungroup() %>%
  #dplyr::filter(n>1) %>% 
  arrange(desc(n)) %>% 
  dplyr::filter(references_title != "The Apportionment of Human Diversity")# %>%

selected_co_refs <- table(top_co_cites$references_title) %>% data.frame %>% dplyr::filter(Freq>2)

selected_co_refs2 <- top_co_cites_overall %>%
  dplyr::filter(!grepl("Molecular Evolutionary Genetics|Mathematical model|SSR", references_title)) %>% # fix list to match top co-cites in original paper
  arrange(desc(n)) %>%
  head(15) %>%
  pull(references_title)

#-----------------------------------------------------------------------------
# Fig 3a: top co-citations
#-----------------------------------------------------------------------------
top_co_cites %>% 
  dplyr::filter(references_title %in% selected_co_refs$Var1) %>%
  dplyr::filter(year>=1995) %>%
  ggplot(aes(x=year, y=n, colour=title_str, group=title_str))+
  geom_point()+
  geom_line()+
  scale_colour_discrete(name="Top co-cited papers")+
  scale_x_continuous(limits=c(1995,2021), breaks=seq(1995,2020, by=5), labels=seq(1995,2020, by=5))+
  ylab("Number of papers in which \n Lewontin, 1972 is co-cited")+
  xlab("Year")+
  theme_classic()+
  theme()

citing_data_df_ref %>% 
  #group_by(title_str, references_title, references_year, references_doi) %>% 
  dplyr::filter(year>=1995) %>%
  group_by(references_authors, references_title, references_year, references_doi) %>% 
  count() %>% 
  dplyr::filter(references_title %in% selected_co_refs2) %>% 
  rowwise() %>% 
  mutate(nauth=nrow(references_authors)) %>% 
  rowwise() %>% 
  mutate(auth=references_authors$name[1]) %>% 
  mutate(auth_final=ifelse(nauth==1, auth, paste0(auth, " et al."))) %>% 
  mutate(auth_final=gsub("[A-Z][.] ", "", auth_final)) %>%
  mutate(title_str = paste0(auth_final, " (", references_year, ") ", references_title)) %>%
  mutate(references_title = factor(references_title, levels=selected_co_refs2)) %>%
  dplyr::filter(!grepl("Jeffrey|Pieter", title_str))

fig3a_dat <- citing_data_df_ref %>% 
  dplyr::filter(references_title %in% selected_co_refs2) %>% 
  dplyr::filter(!grepl("Jeffrey|Pieter", references_authors)) %>%
  rowwise() %>%
  mutate(nauth=nrow(references_authors)) %>% 
  rowwise() %>%
  mutate(auth=references_authors$name[1]) %>% 
  group_by(references_title, auth, nauth, references_year) %>% 
  count() %>% 
  mutate(title=str_to_title(references_title)) %>%
  # rowwise() %>% 
  # mutate(auth=references_authors$name[1]) %>% 
  mutate(auth_final=ifelse(nauth==1, auth, paste0(auth, " et al."))) %>% 
  mutate(auth_final=gsub("[A-Z][.] ", "", auth_final)) %>%
  mutate(title_str = paste0(auth_final, " (", references_year, ") ", title)) %>%
  mutate(category=ifelse(grepl("Nei|Kimura", title_str), "same-era popgen papers",
                         ifelse(grepl("Excoffier|Rosenberg|Barbujani", title_str), "replicating papers",
                                ifelse(grepl("Rohlf|Pritchard|Peakall|Yeh", title_str), "popgen software",
                                       ifelse(grepl("Williams|Vos", title_str), "new genetic markers", "other"))))) %>%
  arrange(category, desc(n))

fig3a <- fig3a_dat %>%
  mutate(title_str = factor(title_str, levels=fig3a_dat$title_str[1:15])) %>%
  dplyr::filter(!grepl("Jeffrey|Pieter", title_str)) %>%
  dplyr::filter(!is.na(title_str)) %>%
  arrange(desc(n)) %>% 
  ggplot(aes(x=title_str, y=n, label=str_wrap(title_str, 20), fill=category))+
  geom_col(position="dodge")+
  scale_fill_brewer(palette="Set1")+
  scale_x_discrete(expand=c(0,0), breaks=fig3a_dat$title_str[c(1,3,6,10,14)], labels=unique(fig3a_dat$category))+
  scale_y_continuous(expand=c(0,0))+
  geom_label_repel(box.padding = 2, 
                   size=2, 
                   min.segment.length = 0,
                   seed = 42,
                   show.legend = FALSE)+
  ylab("Number of co-citing papers")+
  ggtitle("a")+
  # guides(text="none")+
  # facet_wrap(~category, nrow=1, scales="free_x")+
  theme_classic()+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y = element_text(size=16),
        legend.position="none")+
  NULL


  

# fig3a <- citing_data_df_ref %>% 
#   #group_by(title_str, references_title, references_year, references_doi) %>% 
#   group_by(year, references_authors, references_title, references_year, references_doi) %>% 
#   count() %>% 
#   group_by(year) %>% 
#   mutate(tot_refs=sum(n)) %>% 
#   dplyr::filter(references_title %in% selected_co_refs2) %>% 
#   rowwise() %>% 
#   mutate(nauth=nrow(references_authors)) %>% 
#   rowwise() %>% 
#   mutate(auth=references_authors$name[1]) %>% 
#   mutate(auth_final=ifelse(nauth==1, auth, paste0(auth, " et al."))) %>% 
#   mutate(auth_final=gsub("[A-Z][.] ", "", auth_final)) %>%
#   mutate(title_str = paste0(auth_final, " (", references_year, ") ", references_title)) %>%
#   mutate(references_title = factor(references_title, levels=selected_co_refs2)) %>%
#   dplyr::filter(year>=1995) %>%
#   dplyr::filter(!grepl("Jeffrey|Pieter", title_str)) %>%
#   group_by(references_title, year) %>%
#   arrange(desc(n)) %>%
#   slice(1L) %>%
#   #ggplot(aes(x=year, y=n, colour=paste0(references_title, " (", references_year, ")")))+
#   ggplot(aes(x=year, y=n, colour=references_title))+
#   geom_point()+
#   geom_line()+
#   scale_colour_discrete(name="Top co-cited papers")+
#   # facet_wrap(~title_str, nrow=5, labeller = labeller(title_str = label_wrap_gen(width = 50)))+
#   # facet_wrap(~title_str, ncol=1, labeller = labeller(title_str = label_wrap_gen(width = 50)))+
#   scale_x_continuous(limits=c(1995,2021), breaks=seq(1995,2020, by=5), labels=seq(1995,2020, by=5))+
#   ylab("Number of papers in which \n Lewontin, 1972 is co-cited")+
#   xlab("Year")+
#   theme_classic()+
#   theme(legend.position="none")


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

fig3b <- bind_rows(l1972_citations, 
          n1972_citations, 
          n1973_citations, 
          n1978_citations, 
          k1964_citations,
          # nr1972_citations,
          # nr1974_citations,
          .id="paper") %>%
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
  mutate(paper=factor(paper, levels = c("Lewontin, 1972", "Nei, 1972", "Nei, 1973", "Nei, 1978", "Kimura & Crow, 1964"))) %>%
  ggplot(aes(x=citations_year, y=norm_cite_rate, colour=paper, shape=paper))+
  geom_point()+
  geom_line()+
  scale_colour_manual(values=c("red", rep("grey60", 4))) +
  scale_x_continuous(breaks=seq(1965,2020, by=10), labels=seq(1965,2020, by=10))+
  ylab("fraction of total citations per paper")+
  xlab("Year")+
  ggtitle("b")+
  theme_classic()+
  theme(legend.position = c(0.3, 0.8),
        legend.title=element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size=12, angle=45, hjust=1),
        axis.text.y = element_text(size=12),
      axis.title.y = element_text(size=16))+
  NULL

fig3c <- bind_rows(l1972_citations, 
          nr1972_citations,
          nr1974_citations,
          .id="paper") %>%
  mutate(paper=recode(paper,
                      "1" = "Lewontin, 1972",
                      "2" = "Nei & Roychoudhury, 1972",
                      "3" = "Nei & Roychoudhury, 1974")) %>%
  group_by(citations_year, paper) %>%
  count() %>%
  group_by(paper) %>%
  mutate(tot = sum(n)) %>%
  rowwise() %>%
  mutate(norm_cite_rate = n/tot) %>%
  ungroup() %>%
  dplyr::filter(citations_year<2005) %>%
  mutate(paper=factor(paper, levels = c("Lewontin, 1972",
                                        "Nei & Roychoudhury, 1972",
                                        "Nei & Roychoudhury, 1974"))) %>%
  ggplot(aes(x=citations_year, y=n, colour=paper, shape=paper))+
  geom_point()+
  geom_line()+
  scale_colour_manual(values=c("red", rep("grey60", 4))) +
  scale_x_continuous(breaks=seq(1965,2020, by=5), labels=seq(1965,2020, by=5))+
  ylab("Number of citations")+
  ggtitle("c")+
  theme_classic()+
  theme(legend.position = c(0.4, 0.8),
        legend.title=element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size=12, angle=45, hjust=1),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=16))+
  NULL

grid.arrange(
  grobs = list(fig3a, fig3b, fig3c),
  widths = c(1, 1),
  layout_matrix = rbind(c(1, 1),
                        c(2, 3))
)

# ggarrange(fig3a, fig3b, fig3c, ncol=2)

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


#Blood groups/proteins,Introduction of genetic markers,1960-01-01,1972-01-01,#c8e6c9
data <- read.csv(text="event,group,start,end,color
                       The Genetic Basis of Evolutionary Change,Influential citing books,1974-01-01,1974-01-01,#1665c0
                       Sociobiology,Influential citing books,1975-01-01,1975-01-01,#1565c0
                       The Mismeasure of Man,Influential citing books,1981-01-01,1981-01-01,#1565c0
                       The History and Geography of Human Genes,Influential citing books,1994-01-01,1994-01-01,#1565c0
                       Nei (1973),Influential citing papers,1973-01-01,1973-01-01,#1565c0
                       Nei (1977),Influential citing papers,1977-01-01,1977-01-01,#1565c0
                       Rao (1982),Influential citing papers,1982-01-01,1982-01-01,#1565c0
                       Bowcock et al. (1994),Influential citing papers,1994-01-01,1994-01-01,#1565c0
                       Haney Lopez (1994),Influential citing papers,1994-01-01,1994-01-01,#1565c0
                       Lande (1996),Influential citing papers,1996-01-01,1996-01-01,#1565c0
                       Williams et al. (1997),Influential citing papers,1997-01-01,1997-01-01,#1565c0
                       Williams et al. (1999),Influential citing papers,1999-01-01,1999-01-01,#1565c0
                       Rosenberg et al. (2002),Influential citing papers,2002-01-01,2002-01-01,#1565c0
                       Nei & Roychoudhury (1972),Replicating papers,1972-01-01,1972-01-01,#471b04
                       Nei & Roychoudhury (1974),Replicating papers,1974-01-01,1974-01-01,#471b04
                       Nei & Roychoudhury (1982),Replicating papers,1982-01-01,1982-01-01,#471b04
                       Ryman et al. (1983),Replicating papers,1983-01-01,1983-01-01,#471b04
                       Excoffier et al. (1992),Replicating papers,1992-01-01,1992-01-01,#471b04
                       Dean et al. (1994),Replicating papers,1994-01-01,1994-01-01,#471b04
                       Bowcock et al. (1994),Replicating papers,1994-01-01,1994-01-01,#471b04
                       Barbujani et al. (1997),Replicating papers,1997-01-01,1997-01-01,#471b04
                       Rosenberg et al. (2002),Replicating papers,2002-01-01,2002-01-01,#471b04
                      The Genetic Basis of Evolutionary Change,Lewontin's books,1974-01-01,1974-01-01,#524070
                      Biology as Ideology,Lewontin's books,1991-01-01,1991-01-01,#524070 
                      Human Diversity,Lewontin's books,1982-01-01,1982-01-01,#524070
                      Not in our Genes,Lewontin's books,1984-01-01,1984-01-01,#524070 
                      The Dialectical Biologist,Lewontin's books,1985-01-01,1985-01-01,#524070 
                      It Ain't Necessarily So: The Dream of the Human Genome and Other Illusions,Lewontin's books,2000-01-01,2000-01-01,#524070 
                      The Triple Helix,Lewontin's books,1998-01-01,1998-01-01,#524070
                      VNTRs,Introduction of genetic markers,1984-01-01,1984-01-01,#5c7343
                       RFLPs,Introduction of genetic markers,1985-01-01,1985-01-01,#5c7343
                       Microsatellites,Introduction of genetic markers,1989-01-01,1989-01-01,#5c7343
                       RAPDs,Introduction of genetic markers,1990-01-01,1990-01-01,#5c7343
                       AFLPs,Introduction of genetic markers,1995-01-01,1995-01-01,#5c7343
                       SNVs,Introduction of genetic markers,1999-01-01,1999-01-01,#5c7343
                      HGP initiated,Other important events,1991-01-01,1991-01-01,#f44336
                      \"The Bell Curve\" published,Other important events,1994-01-01,1994-01-01,#f44336
                      HGP draft completed,Other important events,2001-01-01,2001-01-01,#f44336
                      Edwards (2003),Other important events,2003-01-01,2003-01-01,#f44336")

# Cross-cultural Psychology: Research and Applications,Influential citing books,1992-01-01,1992-01-01,#1565c0
# ,Lewontin's books,2007-01-01,2007-01-01,#524070 
# Li et al. (2008),Replicating papers,2008-01-01,2008-01-01,#524070
# Jost (2006),Influential citing papers,2006-01-01,2006-01-01,#1565c0
# Jost (2007),Influential citing papers,2007-01-01,2007-01-01,#1565c0
# Jost (2008),Influential citing papers,2008-01-01,2008-01-01,#1565c0
# ,Top co-cited papers*,1964-01-01,1964-01-01,#524070
# ,Top co-cited papers*,1967-01-01,1967-01-01,#524070
# ,Top co-cited papers*,1972-01-01,1972-01-01,#524070
# ,Top co-cited papers*,1973-01-01,1973-01-01,#524070
# ,Top co-cited papers*,1978-01-01,1978-01-01,#524070
# ,Top co-cited papers*,1990-01-01,1990-01-01,#524070
# ,Top co-cited papers*,1990-01-01,1990-01-01,#524070
# ,Top co-cited papers*,1992-01-01,1992-01-01,#524070
# ,Top co-cited papers*,1992-01-01,1992-01-01,#524070
# ,Top co-cited papers*,1995-01-01,1995-01-01,#524070
# ,Top co-cited papers*,1997-01-01,1997-01-01,#524070
# ,Top co-cited papers*,1997-01-01,1997-01-01,#524070
# ,Top co-cited papers*,2000-01-01,2000-01-01,#524070
# ,Top co-cited papers*,2002-01-01,2002-01-01,#524070
# ,Top co-cited papers*,2006-01-01,2006-01-01,#524070

# 'The Bell Curve' published,Other important events,1994-01-01,1994-01-01,#f44336
# Human Genome Project initiated,Other important events,1991-01-01,1991-01-01,#f44336
# First draft of Human Genome published,Other important events,2001-01-01,2001-01-01,#f44336
# Edwards publishes 'Lewontin's Fallacy',Other important events,2003-01-01,2003-01-01,#f44336"

fig6 <- gg_vistime(data, show_labels=FALSE)+
  geom_vline(xintercept=as.POSIXct("1972-01-01"), linetype="dashed")+
  geom_label_repel(aes(label=event), box.padding = 0.5)+
  # scale_x_date(breaks=seq(ISOdate(1975,1,1), ISOdate(2005,1,1), "5 years"))+
  # scale_x_date(breaks=seq(as.Date("1975/1/1"), as.Date("2005/1/1"), "5 years"))+
  # scale_x_date(date_breaks="5 year")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size=12, hjust=1),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=16))+
  NULL

fig6
