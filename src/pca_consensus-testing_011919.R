
setwd("C:/Users/soren/Google Drive/Biomedical MultipliciTs/1. Evidence Infrastructure/Prostate Cancer/Networks/Consensus Testing")

library('RColorBrewer')
library('extrafont')
library('igraph')
library('plyr') 
library('visNetwork')

#detach("package:igraph")
#detach("package:visNetwork")

#creating weights for links in igraph network object - evidence file 
l80 <- read.csv("1980 - Ties.csv",header=TRUE, as.is=TRUE) 
n80 <- read.csv("1980 - Nodes.csv",header=TRUE, as.is=TRUE)

# making igraph object 
g80 <- graph_from_data_frame(d=l80, vertices=n80, directed=T) 

# cutting network down based on median
g80 <- induced.subgraph(g80, V(g80)[ Year %in% c("1976", "1977", "1978", "1979", "1980") ])

#measuring modularity 
m80 <- cluster_infomap(g80)
modularity(m80)
c80 <- cluster_leading_eigen(g80)
modularity(c80)
gsize(g80)

#1981
l81 <- read.csv("1981 - Ties.csv",header=TRUE, as.is=TRUE) 
n81 <- read.csv("1981 - Nodes.csv",header=TRUE, as.is=TRUE)
g81 <- graph_from_data_frame(d=l81, vertices=n81, directed=T) 
g81 <- induced.subgraph(g81, V(g81)[ Year %in% c("1976", "1977", "1978", "1979", "1980", "1981") ])
m81 <- cluster_infomap(g81)
modularity(m81)
gsize(g81)

#1982
l82 <- read.csv("1982 - Ties.csv",header=TRUE, as.is=TRUE) 
n82 <- read.csv("1982 - Nodes.csv",header=TRUE, as.is=TRUE)
g82 <- graph_from_data_frame(d=l82, vertices=n82, directed=T) 
g82 <- induced.subgraph(g82, V(g82)[ Year %in% c("1977", "1978", "1979", "1980", "1981", "1982") ])
m82 <- cluster_infomap(g82)
modularity(m82)
gsize(g82)

#1983
l83 <- read.csv("1983 - Ties.csv",header=TRUE, as.is=TRUE) 
n83 <- read.csv("1983 - Nodes.csv",header=TRUE, as.is=TRUE)
g83 <- graph_from_data_frame(d=l83, vertices=n83, directed=T) 
g83 <- induced.subgraph(g83, V(g83)[ Year %in% c("1978", "1979", "1980", "1981", "1982", "1983") ])
m83 <- cluster_infomap(g83)
modularity(m83)
gsize(g83)

#1984
l84 <- read.csv("1984 - Ties.csv",header=TRUE, as.is=TRUE) 
n84 <- read.csv("1984 - Nodes.csv",header=TRUE, as.is=TRUE)
g84 <- graph_from_data_frame(d=l84, vertices=n84, directed=T) 
g84 <- induced.subgraph(g84, V(g84)[ Year %in% c("1979", "1980", "1981", "1982", "1983", "1984") ])
m84 <- cluster_infomap(g84)
modularity(m84)
gsize(g84)

#1985
l85 <- read.csv("1985 - Ties.csv",header=TRUE, as.is=TRUE) 
n85 <- read.csv("1985 - Nodes.csv",header=TRUE, as.is=TRUE)
g85 <- graph_from_data_frame(d=l85, vertices=n85, directed=T) 
g85 <- induced.subgraph(g85, V(g85)[ Year %in% c("1980", "1981", "1982", "1983", "1984", "1985") ])
m85 <- cluster_infomap(g85)
modularity(m85)
gsize(g85)

#1986
l86 <- read.csv("1986 - Ties.csv",header=TRUE, as.is=TRUE) 
n86 <- read.csv("1986 - Nodes.csv",header=TRUE, as.is=TRUE)
g86 <- graph_from_data_frame(d=l86, vertices=n86, directed=T) 
g86 <- induced.subgraph(g86, V(g86)[ Year %in% c("1980", "1981", "1982", "1983", "1984", "1985", "1986") ])
m86 <- cluster_infomap(g86)
modularity(m86)
gsize(g86)

#1987
l87 <- read.csv("1987 - Ties.csv",header=TRUE, as.is=TRUE) 
n87 <- read.csv("1987 - Nodes.csv",header=TRUE, as.is=TRUE)
g87 <- graph_from_data_frame(d=l87, vertices=n87, directed=T) 
g87 <- induced.subgraph(g87, V(g87)[ Year %in% c("1982", "1983", "1984", "1985", "1986", "1987") ])
m87 <- cluster_infomap(g87)
modularity(m87)
gsize(g87)

#1988
l88 <- read.csv("1988 - Ties.csv",header=TRUE, as.is=TRUE) 
n88 <- read.csv("1988 - Nodes.csv",header=TRUE, as.is=TRUE)
g88 <- graph_from_data_frame(d=l88, vertices=n88, directed=T) 
g88 <- induced.subgraph(g88, V(g88)[ Year %in% c("1982", "1983", "1984", "1985", "1986", "1987", "1988") ])
m88 <- cluster_infomap(g88)
modularity(m88)
gsize(g88)

#1989
l89 <- read.csv("1989 - Ties.csv",header=TRUE, as.is=TRUE) 
n89 <- read.csv("1989 - Nodes.csv",header=TRUE, as.is=TRUE)
g89 <- graph_from_data_frame(d=l89, vertices=n89, directed=T) 
g89 <- induced.subgraph(g89, V(g89)[ Year %in% c("1983", "1984", "1985", "1986", "1987", "1988", "1989") ])
m89 <- cluster_infomap(g89)
modularity(m89)
gsize(g89)

#1990
l90 <- read.csv("1990 - Ties.csv",header=TRUE, as.is=TRUE) 
n90 <- read.csv("1990 - Nodes.csv",header=TRUE, as.is=TRUE)
g90 <- graph_from_data_frame(d=l90, vertices=n90, directed=T) 
g90 <- induced.subgraph(g90, V(g90)[ Year %in% c("1984", "1985", "1986", "1987", "1988", "1989", "1990") ])
m90 <- cluster_infomap(g90)
modularity(m90)
gsize(g90)

#1991
l91 <- read.csv("1991 - Ties.csv",header=TRUE, as.is=TRUE) 
n91 <- read.csv("1991 - Nodes.csv",header=TRUE, as.is=TRUE)
g91 <- graph_from_data_frame(d=l91, vertices=n91, directed=T) 
g91 <- induced.subgraph(g91, V(g91)[ Year %in% c("1985", "1986", "1987", "1988", "1989", "1990", "1991") ])
m91 <- cluster_infomap(g91)
modularity(m91)
gsize(g91)

#1992
l92 <- read.csv("1992 - Ties.csv",header=TRUE, as.is=TRUE) 
n92 <- read.csv("1992 - Nodes.csv",header=TRUE, as.is=TRUE)
g92 <- graph_from_data_frame(d=l92, vertices=n92, directed=T) 
g92 <- induced.subgraph(g92, V(g92)[ Year %in% c("1986", "1987", "1988", "1989", "1990", "1991", "1992") ])
m92 <- cluster_infomap(g92)
modularity(m92)
gsize(g92)

#1993
l93 <- read.csv("1993 - Ties.csv",header=TRUE, as.is=TRUE) 
n93 <- read.csv("1993 - Nodes.csv",header=TRUE, as.is=TRUE)
g93 <- graph_from_data_frame(d=l93, vertices=n93, directed=T) 
g93 <- induced.subgraph(g93, V(g93)[ Year %in% c("1987", "1988", "1989", "1990", "1991", "1992", "1993") ])
m93 <- cluster_infomap(g93)
modularity(m93)
gsize(g93)

#1994
l94 <- read.csv("1994 - Ties.csv",header=TRUE, as.is=TRUE) 
n94 <- read.csv("1994 - Nodes.csv",header=TRUE, as.is=TRUE)
g94 <- graph_from_data_frame(d=l94, vertices=n94, directed=T) 
g94 <- induced.subgraph(g94, V(g94)[ Year %in% c("1988", "1989", "1990", "1991", "1992", "1993", "1994") ])
m94 <- cluster_infomap(g94)
modularity(m94)
gsize(g94)

#1995
l95 <- read.csv("1995 - Ties.csv",header=TRUE, as.is=TRUE) 
n95 <- read.csv("1995 - Nodes.csv",header=TRUE, as.is=TRUE)
g95 <- graph_from_data_frame(d=l95, vertices=n95, directed=T) 
g95 <- induced.subgraph(g95, V(g95)[ Year %in% c("1989", "1990", "1991", "1992", "1993", "1994", "1995") ])
m95 <- cluster_infomap(g95)
modularity(m95)
gsize(g95)

#1996 
l96 <- read.csv("1996 - Ties.csv",header=TRUE, as.is=TRUE) 
n96 <- read.csv("1996 - Nodes.csv",header=TRUE, as.is=TRUE)
g96 <- graph_from_data_frame(d=l96, vertices=n96, directed=T) 
g96 <- induced.subgraph(g96, V(g96)[ Year %in% c("1990", "1991", "1992", "1993", "1994", "1995", "1996") ])
m96 <- cluster_infomap(g96)
modularity(m96)
gsize(g96)

#1997
l97 <- read.csv("1997 - Ties.csv",header=TRUE, as.is=TRUE) 
n97 <- read.csv("1997 - Nodes.csv",header=TRUE, as.is=TRUE)
g97 <- graph_from_data_frame(d=l97, vertices=n97, directed=T) 
g97 <- induced.subgraph(g97, V(g97)[ Year %in% c("1991", "1992", "1993", "1994", "1995", "1996", "1997") ])
m97 <- cluster_infomap(g97)
modularity(m97)
gsize(g97)

#1998
l98 <- read.csv("1998 - Ties.csv",header=TRUE, as.is=TRUE) 
n98 <- read.csv("1998 - Nodes.csv",header=TRUE, as.is=TRUE)
g98 <- graph_from_data_frame(d=l98, vertices=n98, directed=T) 
g98 <- induced.subgraph(g98, V(g98)[ Year %in% c("1993", "1994", "1995", "1996", "1997", "1998") ])
m98 <- cluster_infomap(g98)
modularity(m98)
gsize(g98)

#1999
l99 <- read.csv("1999 - Ties.csv",header=TRUE, as.is=TRUE) 
n99 <- read.csv("1999 - Nodes.csv",header=TRUE, as.is=TRUE)
g99 <- graph_from_data_frame(d=l99, vertices=n99, directed=T) 
g99 <- induced.subgraph(g99, V(g99)[ Year %in% c("1993", "1994", "1995", "1996", "1997", "1998", "1999") ])
m99 <- cluster_infomap(g99)
modularity(m99)
gsize(g99)

#2000
l00 <- read.csv("2000 - Ties.csv",header=TRUE, as.is=TRUE) 
n00 <- read.csv("2000 - Nodes.csv",header=TRUE, as.is=TRUE)
g00 <- graph_from_data_frame(d=l00, vertices=n00, directed=T) 
g00 <- induced.subgraph(g00, V(g00)[ Year %in% c("1994", "1995","1996", "1997","1998","1999","2000") ])
m00 <- cluster_infomap(g00)
modularity(m00)
gsize(g00)

#2001
l01 <- read.csv("2001 - Ties.csv",header=TRUE, as.is=TRUE) 
n01 <- read.csv("2001 - Nodes.csv",header=TRUE, as.is=TRUE)
g01 <- graph_from_data_frame(d=l01, vertices=n01, directed=T) 
g01 <- induced.subgraph(g01, V(g01)[ Year %in% c("1994", "1995","1996", "1997","1998","1999","2000", "2001") ])
m01 <- cluster_infomap(g01)
modularity(m01)
gsize(g01)

#2002
l02 <- read.csv("2002 - Ties.csv",header=TRUE, as.is=TRUE) 
n02 <- read.csv("2002 - Nodes.csv",header=TRUE, as.is=TRUE)
g02 <- graph_from_data_frame(d=l02, vertices=n02, directed=T) 
g02 <- induced.subgraph(g02, V(g02)[ Year %in% c("1996", "1997","1998","1999","2000", "2001", "2002") ])
m02 <- cluster_infomap(g02)
modularity(m02)
gsize(g02)

#2003
l03 <- read.csv("2003 - Ties.csv",header=TRUE, as.is=TRUE) 
n03 <- read.csv("2003 - Nodes.csv",header=TRUE, as.is=TRUE)
g03 <- graph_from_data_frame(d=l03, vertices=n03, directed=T) 
g03 <- induced.subgraph(g03, V(g03)[ Year %in% c("1997","1998","1999","2000", "2001", "2002", "2003") ])
m03 <- cluster_infomap(g03)
modularity(m03)
gsize(g03)

#2004
l04 <- read.csv("2004 - Ties.csv",header=TRUE, as.is=TRUE) 
n04 <- read.csv("2004 - Nodes.csv",header=TRUE, as.is=TRUE)
g04 <- graph_from_data_frame(d=l04, vertices=n04, directed=T) 
g04 <- induced.subgraph(g04, V(g04)[ Year %in% c("1998","1999","2000", "2001", "2002", "2003", "2004") ])
m04 <- cluster_infomap(g04)
modularity(m04)
gsize(g04)

#2005
l05 <- read.csv("2005 - Ties.csv",header=TRUE, as.is=TRUE) 
n05 <- read.csv("2005 - Nodes.csv",header=TRUE, as.is=TRUE)
g05 <- graph_from_data_frame(d=l05, vertices=n05, directed=T) 
g05 <- induced.subgraph(g05, V(g05)[ Year %in% c("1999","2000","2001","2002","2003","2004","2005") ])
m05 <- cluster_infomap(g05)
modularity(m05)
gsize(g05)

#2006
l06 <- read.csv("2006 - Ties.csv",header=TRUE, as.is=TRUE) 
n06 <- read.csv("2006 - Nodes.csv",header=TRUE, as.is=TRUE)
g06 <- graph_from_data_frame(d=l06, vertices=n06, directed=T) 
g06 <- induced.subgraph(g06, V(g06)[ Year %in% c("2000","2001","2002","2003","2004","2005","2006") ])
m06 <- cluster_infomap(g06)
modularity(m06)
gsize(g06)

#2007
l07 <- read.csv("2007 - Ties.csv",header=TRUE, as.is=TRUE) 
n07 <- read.csv("2007 - Nodes.csv",header=TRUE, as.is=TRUE)
g07 <- graph_from_data_frame(d=l07, vertices=n07, directed=T) 
g07 <- induced.subgraph(g07, V(g07)[ Year %in% c("2002","2003","2004","2005","2006","2007") ])
m07 <- cluster_infomap(g07)
modularity(m07)
gsize(g07)

#2008
l08 <- read.csv("2008 - Ties.csv",header=TRUE, as.is=TRUE) 
n08 <- read.csv("2008 - Nodes.csv",header=TRUE, as.is=TRUE)
g08 <- graph_from_data_frame(d=l08, vertices=n08, directed=T) 
g08 <- induced.subgraph(g08, V(g08)[ Year %in% c("2003","2004","2005","2006","2007","2008") ])
m08 <- cluster_infomap(g08)
modularity(m08)
gsize(g08)

#2009
l09 <- read.csv("2009 - Ties.csv",header=TRUE, as.is=TRUE) 
n09 <- read.csv("2009 - Nodes.csv",header=TRUE, as.is=TRUE)
g09 <- graph_from_data_frame(d=l09, vertices=n09, directed=T) 
g09 <- induced.subgraph(g09, V(g09)[ Year %in% c("2003","2004","2005","2006","2007","2008","2009") ])
m09 <- cluster_infomap(g09)
modularity(m09)
gsize(g09)

#2010
l10 <- read.csv("2010 - Ties.csv",header=TRUE, as.is=TRUE) 
n10 <- read.csv("2010 - Nodes.csv",header=TRUE, as.is=TRUE)
g10 <- graph_from_data_frame(d=l10, vertices=n10, directed=T) 
g10 <- induced.subgraph(g10, V(g10)[ Year %in% c("2003","2004","2005","2006","2007","2008","2009","2010") ])
m10 <- cluster_infomap(g10)
modularity(m10)
gsize(g10)

#2011
l11 <- read.csv("2011 - Ties.csv",header=TRUE, as.is=TRUE) 
n11 <- read.csv("2011 - Nodes.csv",header=TRUE, as.is=TRUE)
g11 <- graph_from_data_frame(d=l11, vertices=n11, directed=T) 
g11 <- induced.subgraph(g11, V(g11)[ Year %in% c("2004","2005","2006","2007","2008","2009","2010","2011") ])
m11 <- cluster_infomap(g11)
modularity(m11)
gsize(g11)

#2012
l12 <- read.csv("2012 - Ties.csv",header=TRUE, as.is=TRUE) 
n12 <- read.csv("2012 - Nodes.csv",header=TRUE, as.is=TRUE)
g12 <- graph_from_data_frame(d=l12, vertices=n12, directed=T) 
g12 <- induced.subgraph(g12, V(g12)[ Year %in% c("2005","2006","2007","2008","2009","2010","2011","2012") ])
m12 <- cluster_infomap(g12)
modularity(m12)
gsize(g12)

#2013
l13 <- read.csv("2013 - Ties.csv",header=TRUE, as.is=TRUE) 
n13 <- read.csv("2013 - Nodes.csv",header=TRUE, as.is=TRUE)
g13 <- graph_from_data_frame(d=l13, vertices=n13, directed=T) 
g13 <- induced.subgraph(g13, V(g13)[ Year %in% c("2006","2007","2008","2009","2010","2012","2013") ])
m13 <- cluster_infomap(g13)
modularity(m13)
gsize(g13)

#2014
l14 <- read.csv("2014 - Ties.csv",header=TRUE, as.is=TRUE) 
n14 <- read.csv("2014 - Nodes.csv",header=TRUE, as.is=TRUE)
g14 <- graph_from_data_frame(d=l14, vertices=n14, directed=T) 
g14 <- induced.subgraph(g14, V(g14)[ Year %in% c("2007","2008","2009","2010","2011","2012","2013","2014") ])
m14 <- cluster_infomap(g14)
modularity(m14)
gsize(g14)

#2015
l15 <- read.csv("2015 - Ties.csv",header=TRUE, as.is=TRUE) 
n15 <- read.csv("2015 - Nodes.csv",header=TRUE, as.is=TRUE)
g15 <- graph_from_data_frame(d=l15, vertices=n15, directed=T) 
g15 <- induced.subgraph(g15, V(g15)[ Year %in% c("2008","2009","2010","2011","2012","2013","2014","2015") ])
m15 <- cluster_infomap(g15)
modularity(m15)
gsize(g15)

#2016
l16 <- read.csv("2016 - Ties.csv",header=TRUE, as.is=TRUE) 
n16 <- read.csv("2016 - Nodes.csv",header=TRUE, as.is=TRUE)
g16 <- graph_from_data_frame(d=l16, vertices=n16, directed=T) 
g16 <- induced.subgraph(g16, V(g16)[ Year %in% c("2009","2010","2011","2012","2013","2014","2015","2016") ])
m16 <- cluster_infomap(g16)
modularity(m16)
gsize(g16)

#2017
l17 <- read.csv("2017 - Ties.csv",header=TRUE, as.is=TRUE) 
n17 <- read.csv("2017 - Nodes.csv",header=TRUE, as.is=TRUE)
g17 <- graph_from_data_frame(d=l17, vertices=n17, directed=T) 
g17 <- induced.subgraph(g17, V(g17)[ Year %in% c("2009","2010","2011","2012","2013","2014","2015","2016","2017") ])
m17 <- cluster_infomap(g17)
modularity(m17)
gsize(g17)

#modularity analyses 
modularity(m80)
modularity(m81)
modularity(m82)
modularity(m83)
modularity(m84)
modularity(m85)
modularity(m86)
modularity(m87)
modularity(m88)
modularity(m89)
modularity(m90)
modularity(m91)
modularity(m92)
modularity(m93)
modularity(m94)
modularity(m95)
modularity(m96)
modularity(m97)
modularity(m98)
modularity(m99)
modularity(m00)
modularity(m01)
modularity(m02)
modularity(m03)
modularity(m04)
modularity(m05)
modularity(m06)
modularity(m07)
modularity(m08)
modularity(m09)
modularity(m10)
modularity(m11)
modularity(m12)
modularity(m13)
modularity(m14)
modularity(m15)
modularity(m16)
modularity(m17)

# checking graph sizes
gsize(g80)
gsize(g81)
gsize(g82)
gsize(g83)
gsize(g84)
gsize(g85)
gsize(g86)
gsize(g87)
gsize(g88)
gsize(g89)
gsize(g90)
gsize(g91)
gsize(g92)
gsize(g93)
gsize(g94)
gsize(g95)
gsize(g96)
gsize(g97)
gsize(g98)
gsize(g99)
gsize(g00)
gsize(g01)
gsize(g02)
gsize(g03)
gsize(g04)
gsize(g05)
gsize(g06)
gsize(g07)
gsize(g08)
gsize(g09)
gsize(g10)
gsize(g11)
gsize(g12)
gsize(g13)
gsize(g14)
gsize(g15)
gsize(g16)
gsize(g17)

c80 <- cluster_leading_eigen(g80)
modularity(c80)
c81 <- cluster_leading_eigen(g81)
modularity(c81)
c82 <- cluster_leading_eigen(g82)
modularity(c82)
c83 <- cluster_leading_eigen(g83)
modularity(c83)
c84 <- cluster_leading_eigen(g84)
modularity(c84)
c85 <- cluster_leading_eigen(g85)
modularity(c85)
c86 <- cluster_leading_eigen(g86)
modularity(c86)
c87 <- cluster_leading_eigen(g87)
modularity(c87)
c88 <- cluster_leading_eigen(g88)
modularity(c88)
c89 <- cluster_leading_eigen(g89)
modularity(c89)
c90 <- cluster_leading_eigen(g90)
modularity(c90)
c91 <- cluster_leading_eigen(g91)
modularity(c91)
c92 <- cluster_leading_eigen(g92)
modularity(c92)
c93 <- cluster_leading_eigen(g93)
modularity(c93)
c94 <- cluster_leading_eigen(g94)
modularity(c94)
c95 <- cluster_leading_eigen(g95)
modularity(c95)
c96 <- cluster_leading_eigen(g96)
modularity(c96)
c97 <- cluster_leading_eigen(g97)
modularity(c97)
c98 <- cluster_leading_eigen(g98)
modularity(c98)
c99 <- cluster_leading_eigen(g99)
modularity(c99)
c00 <- cluster_leading_eigen(g00)
modularity(c00)
c01 <- cluster_leading_eigen(g01)
modularity(c01)
c02 <- cluster_leading_eigen(g02)
modularity(c02)
c03 <- cluster_leading_eigen(g03)
modularity(c03)
c04 <- cluster_leading_eigen(g04)
modularity(c04)
c05 <- cluster_leading_eigen(g05)
modularity(c05)
c06 <- cluster_leading_eigen(g06)
modularity(c06)
c07 <- cluster_leading_eigen(g07)
modularity(c07)
c08 <- cluster_leading_eigen(g08)
modularity(c08)
c09 <- cluster_leading_eigen(g09)
modularity(c09)
c10 <- cluster_leading_eigen(g10, options=list(maxiter=40000))
modularity(c10)
c11 <- cluster_leading_eigen(g11)
modularity(c11)
c12 <- cluster_leading_eigen(g12, options=list(maxiter=40000))
modularity(c12)
c13 <- cluster_leading_eigen(g13)
modularity(c13)
c14 <- cluster_leading_eigen(g14)
modularity(c14)
c15 <- cluster_leading_eigen(g15, options=list(maxiter=40000))
modularity(c15)
c16 <- cluster_leading_eigen(g16, options=list(maxiter=40000))
modularity(c16)
c17 <- cluster_leading_eigen(g17, options=list(maxiter=40000))
modularity(c17)


# testing 

c80m <- cluster_leading_eigen(g80)$membership
c81m <- cluster_leading_eigen(g81)$membership
c82m <- cluster_leading_eigen(g82)$membership
c83m <- cluster_leading_eigen(g83)$membership
c84m <- cluster_leading_eigen(g84)$membership
c85m <- cluster_leading_eigen(g85)$membership
c86m <- cluster_leading_eigen(g86)$membership
c87m <- cluster_leading_eigen(g87)$membership
c88m <- cluster_leading_eigen(g88)$membership
c89m <- cluster_leading_eigen(g89)$membership
c90m <- cluster_leading_eigen(g90)$membership
c91m <- cluster_leading_eigen(g91)$membership
c92m <- cluster_leading_eigen(g92)$membership
c93m <- cluster_leading_eigen(g93)$membership
c94m <- cluster_leading_eigen(g94)$membership
c95m <- cluster_leading_eigen(g95)$membership
c96m <- cluster_leading_eigen(g96)$membership
c97m <- cluster_leading_eigen(g97)$membership
c98m <- cluster_leading_eigen(g98)$membership
c99m <- cluster_leading_eigen(g99)$membership
c00m <- cluster_leading_eigen(g00)$membership
c01m <- cluster_leading_eigen(g01)$membership
c02m <- cluster_leading_eigen(g02)$membership
c03m <- cluster_leading_eigen(g03)$membership
c04m <- cluster_leading_eigen(g04)$membership
c05m <- cluster_leading_eigen(g05)$membership
c06m <- cluster_leading_eigen(g06)$membership
c07m <- cluster_leading_eigen(g07)$membership
c08m <- cluster_leading_eigen(g08)$membership
c09m <- cluster_leading_eigen(g09)$membership
c10m <- cluster_leading_eigen(g10, options=list(maxiter=40000))$membership
c11m <- cluster_leading_eigen(g11)$membership
c12m <- cluster_leading_eigen(g12, options=list(maxiter=40000))$membership
c13m <- cluster_leading_eigen(g13)$membership
c14m <- cluster_leading_eigen(g14)$membership
c15m <- cluster_leading_eigen(g15, options=list(maxiter=40000))$membership
c16m <- cluster_leading_eigen(g16, options=list(maxiter=40000))$membership
c17m <- cluster_leading_eigen(g17, options=list(maxiter=40000))$membership

max(c80m)
max(c81m)
max(c82m) 
max(c83m) 
max(c84m) 
max(c85m) 
max(c86m)
max(c87m) 
max(c88m) 
max(c89m)
max(c90m) 
max(c91m) 
max(c92m) 
max(c93m) 
max(c94m) 
max(c95m) 
max(c96m)
max(c97m) 
max(c98m) 
max(c99m) 
max(c00m)
max(c01m) 
max(c02m) 
max(c03m) 
max(c04m)
max(c05m) 
max(c06m) 
max(c07m) 
max(c08m)
max(c09m)
max(c10m)
max(c11m) 
max(c12m) 
max(c13m) 
max(c14m) 
max(c15m) 
max(c16m) 
max(c17m) 

e80 <- cluster_edge_betweenness(g80)
modularity(e80)
e81 <- cluster_edge_betweenness(g81)
modularity(e81)
e82 <- cluster_edge_betweenness(g82)
modularity(e82)
e83 <- cluster_edge_betweenness(g83)
modularity(e83)
e84 <- cluster_edge_betweenness(g84)
modularity(e84)
e85 <- cluster_edge_betweenness(g85)
modularity(e85)
e86 <- cluster_edge_betweenness(g86)
modularity(e86)
e87 <- cluster_edge_betweenness(g87)
modularity(e87)
e88 <- cluster_edge_betweenness(g88)
modularity(e88)
e89 <- cluster_edge_betweenness(g89)
modularity(e89)
e90 <- cluster_edge_betweenness(g90)
modularity(e90)
e91 <- cluster_edge_betweenness(g91)
modularity(e91)
e92 <- cluster_edge_betweenness(g92)
modularity(e92)
e93 <- cluster_edge_betweenness(g93)
modularity(e93)
e94 <- cluster_edge_betweenness(g94)
modularity(e94)
e95 <- cluster_edge_betweenness(g95)
modularity(e95)
e96 <- cluster_edge_betweenness(g96)
modularity(e96)
e97 <- cluster_edge_betweenness(g97)
modularity(e97)
e98 <- cluster_edge_betweenness(g98)
modularity(e98)
e99 <- cluster_edge_betweenness(g99)
modularity(e99)
e00 <- cluster_edge_betweenness(g00)
modularity(e00)
e01 <- cluster_edge_betweenness(g01)
modularity(e01)
e02 <- cluster_edge_betweenness(g02)
modularity(e02)
e03 <- cluster_edge_betweenness(g03)
modularity(e03)
e04 <- cluster_edge_betweenness(g04)
modularity(e04)
e05 <- cluster_edge_betweenness(g05)
modularity(e05)
e06 <- cluster_edge_betweenness(g06)
modularity(e06)
e07 <- cluster_edge_betweenness(g07)
modularity(e07)
e08 <- cluster_edge_betweenness(g08)
modularity(e08)
e09 <- cluster_edge_betweenness(g09)
modularity(e09)
e10 <- cluster_edge_betweenness(g10) #options=list(maxiter=40000)
modularity(e10)
e11 <- cluster_edge_betweenness(g11)
modularity(e11)
e12 <- cluster_edge_betweenness(g12) #options=list(maxiter=40000))
modularity(e12)
e13 <- cluster_edge_betweenness(g13)
modularity(e13)
e14 <- cluster_edge_betweenness(g14)
modularity(e14)
e15 <- cluster_edge_betweenness(g15) #options=list(maxiter=40000))
modularity(e15)
e16 <- cluster_edge_betweenness(g16) #options=list(maxiter=40000))
modularity(e16)
e17 <- cluster_edge_betweenness(g17) #options=list(maxiter=40000))
modularity(e17)

e80m <- cluster_edge_betweenness(g80)$membership
e81m <- cluster_edge_betweenness(g81)$membership
e82m <- cluster_edge_betweenness(g82)$membership
e83m <- cluster_edge_betweenness(g83)$membership
e84m <- cluster_edge_betweenness(g84)$membership
e85m <- cluster_edge_betweenness(g85)$membership
e86m <- cluster_edge_betweenness(g86)$membership
e87m <- cluster_edge_betweenness(g87)$membership
e88m <- cluster_edge_betweenness(g88)$membership
e89m <- cluster_edge_betweenness(g89)$membership
e90m <- cluster_edge_betweenness(g90)$membership
e91m <- cluster_edge_betweenness(g91)$membership
e92m <- cluster_edge_betweenness(g92)$membership
e93m <- cluster_edge_betweenness(g93)$membership
e94m <- cluster_edge_betweenness(g94)$membership
e95m <- cluster_edge_betweenness(g95)$membership
e96m <- cluster_edge_betweenness(g96)$membership
e97m <- cluster_edge_betweenness(g97)$membership
e98m <- cluster_edge_betweenness(g98)$membership
e99m <- cluster_edge_betweenness(g99)$membership
e00m <- cluster_edge_betweenness(g00)$membership
e01m <- cluster_edge_betweenness(g01)$membership
e02m <- cluster_edge_betweenness(g02)$membership
e03m <- cluster_edge_betweenness(g03)$membership
e04m <- cluster_edge_betweenness(g04)$membership
e05m <- cluster_edge_betweenness(g05)$membership
e06m <- cluster_edge_betweenness(g06)$membership
e07m <- cluster_edge_betweenness(g07)$membership
e08m <- cluster_edge_betweenness(g08)$membership
e09m <- cluster_edge_betweenness(g09)$membership
e10m <- cluster_edge_betweenness(g10, options=list(maxiter=40000))$membership
e11m <- cluster_edge_betweenness(g11)$membership
e12m <- cluster_edge_betweenness(g12, options=list(maxiter=40000))$membership
e13m <- cluster_edge_betweenness(g13)$membership
e14m <- cluster_edge_betweenness(g14)$membership
e15m <- cluster_edge_betweenness(g15, options=list(maxiter=40000))$membership
e16m <- cluster_edge_betweenness(g16, options=list(maxiter=40000))$membership
e17m <- cluster_edge_betweenness(g17, options=list(maxiter=40000))$membership

max(e80m)
max(e81m)
max(e82m) 
max(e83m) 
max(e84m) 
max(e85m) 
max(e86m)
max(e87m) 
max(e88m) 
max(e89m)
max(e90m) 
max(e91m) 
max(e92m) 
max(e93m) 
max(e94m) 
max(e95m) 
max(e96m)
max(e97m) 
max(e98m) 
max(e99m) 
max(e00m)
max(e01m) 
max(e02m) 
max(e03m) 
max(e04m)
max(e05m) 
max(e06m) 
max(e07m) 
max(e08m)
max(e09m)
max(e10m)
max(e11m) 
max(e12m) 
max(e13m) 
max(e14m) 
max(e15m) 
max(e16m) 
max(e17m)

f80 <- cluster_fast_greedy(g80)
modularity(f80)
f81 <- cluster_fast_greedy(g81)
modularity(f81)
f82 <- cluster_fast_greedy(g82)
modularity(f82)
f83 <- cluster_fast_greedy(g83)
modularity(f83)
f84 <- cluster_fast_greedy(g84)
modularity(f84)
f85 <- cluster_fast_greedy(g85)
modularity(f85)
f86 <- cluster_fast_greedy(g86)
modularity(f86)
f87 <- cluster_fast_greedy(g87)
modularity(f87)
f88 <- cluster_fast_greedy(g88)
modularity(f88)
f89 <- cluster_fast_greedy(g89)
modularity(f89)
f90 <- cluster_fast_greedy(g90)
modularity(f90)
f91 <- cluster_fast_greedy(g91)
modularity(f91)
f92 <- cluster_fast_greedy(g92)
modularity(f92)
f93 <- cluster_fast_greedy(g93)
modularity(f93)
f94 <- cluster_fast_greedy(g94)
modularity(f94)
f95 <- cluster_fast_greedy(g95)
modularity(f95)
f96 <- cluster_fast_greedy(g96)
modularity(f96)
f97 <- cluster_fast_greedy(g97)
modularity(f97)
f98 <- cluster_fast_greedy(g98)
modularity(f98)
f99 <- cluster_fast_greedy(g99)
modularity(f99)
f00 <- cluster_fast_greedy(g00)
modularity(f00)
f01 <- cluster_fast_greedy(g01)
modularity(f01)
f02 <- cluster_fast_greedy(g02)
modularity(f02)
f03 <- cluster_fast_greedy(g03)
modularity(f03)
f04 <- cluster_fast_greedy(g04)
modularity(f04)
f05 <- cluster_fast_greedy(g05)
modularity(f05)
f06 <- cluster_fast_greedy(g06)
modularity(f06)
f07 <- cluster_fast_greedy(g07)
modularity(f07)
f08 <- cluster_fast_greedy(g08)
modularity(f08)
f09 <- cluster_fast_greedy(g09)
modularity(f09)
f10 <- cluster_fast_greedy(g10) #options=list(maxiter=40000)
modularity(f10)
f11 <- cluster_fast_greedy(g11)
modularity(f11)
f12 <- cluster_fast_greedy(g12) #options=list(maxiter=40000))
modularity(f12)
f13 <- cluster_fast_greedy(g13)
modularity(f13)
f14 <- cluster_fast_greedy(g14)
modularity(f14)
f15 <- cluster_fast_greedy(g15) #options=list(maxiter=40000))
modularity(f15)
f16 <- cluster_fast_greedy(g16) #options=list(maxiter=40000))
modularity(f16)
f17 <- cluster_fast_greedy(g17) #options=list(maxiter=40000))
modularity(f17)
