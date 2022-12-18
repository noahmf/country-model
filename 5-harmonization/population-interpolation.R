
pop_interp <- function(object){
  object1 <- object %>% 
    group_by(country_name) %>% 
    mutate(
      pop= # linear interpolation for all countries for years 1-1710
        ifelse(year%in%seq(50,1150,100), round((lag(pop,1)+lead(pop,1))/2,0), 
               
        ifelse(year%in%seq(1205,1505,100), round((lag(pop,1)*(19/20))+(lead(pop,19)*(1/20)),0), 
        ifelse(year%in%seq(1210,1510,100), round((lag(pop,2)*(18/20))+(lead(pop,18)*(2/20)),0), 
        ifelse(year%in%seq(1215,1515,100), round((lag(pop,3)*(17/20))+(lead(pop,17)*(3/20)),0), 
        ifelse(year%in%seq(1220,1520,100), round((lag(pop,4)*(16/20))+(lead(pop,16)*(4/20)),0), 
        ifelse(year%in%seq(1225,1525,100), round((lag(pop,5)*(15/20))+(lead(pop,15)*(5/20)),0), 
        ifelse(year%in%seq(1230,1530,100), round((lag(pop,6)*(14/20))+(lead(pop,14)*(6/20)),0), 
        ifelse(year%in%seq(1235,1535,100), round((lag(pop,7)*(13/20))+(lead(pop,13)*(7/20)),0), 
        ifelse(year%in%seq(1240,1540,100), round((lag(pop,8)*(12/20))+(lead(pop,12)*(8/20)),0), 
        ifelse(year%in%seq(1245,1545,100), round((lag(pop,9)*(11/20))+(lead(pop,11)*(9/20)),0), 
        ifelse(year%in%seq(1250,1550,100), round((lag(pop,10)*(10/20))+(lead(pop,10)*(10/20)),0), 
        ifelse(year%in%seq(1255,1555,100), round((lag(pop,11)*(9/20))+(lead(pop,9)*(11/20)),0), 
        ifelse(year%in%seq(1260,1560,100), round((lag(pop,12)*(8/20))+(lead(pop,8)*(12/20)),0), 
        ifelse(year%in%seq(1265,1565,100), round((lag(pop,13)*(7/20))+(lead(pop,7)*(13/20)),0), 
        ifelse(year%in%seq(1270,1570,100), round((lag(pop,14)*(6/20))+(lead(pop,6)*(14/20)),0), 
        ifelse(year%in%seq(1275,1575,100), round((lag(pop,15)*(5/20))+(lead(pop,5)*(15/20)),0), 
        ifelse(year%in%seq(1280,1580,100), round((lag(pop,16)*(4/20))+(lead(pop,4)*(16/20)),0), 
        ifelse(year%in%seq(1285,1585,100), round((lag(pop,17)*(3/20))+(lead(pop,3)*(17/20)),0), 
        ifelse(year%in%seq(1290,1590,100), round((lag(pop,18)*(2/20))+(lead(pop,2)*(18/20)),0), 
        ifelse(year%in%seq(1295,1595,100), round((lag(pop,19)*(1/20))+(lead(pop,1)*(19/20)),0), 
                                                                                                                                                    
        ifelse(year==1605, round((lag(pop,1)^(21/22))*(lead(pop,21)^(1/22)),0), 
        ifelse(year==1610, round((lag(pop,2)^(20/22))*(lead(pop,20)^(2/22)),0), 
        ifelse(year==1615, round((lag(pop,3)^(19/22))*(lead(pop,19)^(3/22)),0), 
        ifelse(year==1620, round((lag(pop,4)^(18/22))*(lead(pop,18)^(4/22)),0), 
        ifelse(year==1625, round((lag(pop,5)^(17/22))*(lead(pop,17)^(5/22)),0), 
        ifelse(year==1630, round((lag(pop,6)^(16/22))*(lead(pop,16)^(6/22)),0), 
        ifelse(year==1635, round((lag(pop,7)^(15/22))*(lead(pop,15)^(7/22)),0), 
        ifelse(year==1640, round((lag(pop,8)^(14/22))*(lead(pop,14)^(8/22)),0), 
        ifelse(year==1645, round((lag(pop,9)^(13/22))*(lead(pop,13)^(9/22)),0), 
        ifelse(year==1650, round((lag(pop,10)^(12/22))*(lead(pop,12)^(10/22)),0), 
        ifelse(year==1655, round((lag(pop,11)^(11/22))*(lead(pop,11)^(11/22)),0), 
        ifelse(year==1660, round((lag(pop,12)^(10/22))*(lead(pop,10)^(12/22)),0), 
        ifelse(year==1665, round((lag(pop,13)^(9/22))*(lead(pop,9)^(13/22)),0), 
        ifelse(year==1670, round((lag(pop,14)^(8/22))*(lead(pop,8)^(14/22)),0), 
        ifelse(year==1675, round((lag(pop,15)^(7/22))*(lead(pop,7)^(15/22)),0), 
        ifelse(year==1680, round((lag(pop,16)^(6/22))*(lead(pop,6)^(16/22)),0), 
        ifelse(year==1685, round((lag(pop,17)^(5/22))*(lead(pop,5)^(17/22)),0), 
        ifelse(year==1690, round((lag(pop,18)^(4/22))*(lead(pop,4)^(18/22)),0), 
        ifelse(year==1695, round((lag(pop,19)^(3/22))*(lead(pop,3)^(19/22)),0), 
        ifelse(year==1700, round((lag(pop,20)^(2/22))*(lead(pop,2)^(20/22)),0), 
        ifelse(year==1705, round((lag(pop,21)^(1/22))*(lead(pop,1)^(21/22)),0), 
                                                                                                                                                                                                                                                                                                       
              pop))))))))))))))))))))))))))))))))))))))))),
    ) %>% 
    mutate(
      pop= # linear interpolation for all countries but China for 1710-1800; China data is not missing
        ifelse(country_name!="China",
        ifelse(year==1720, round((lag(pop,2)^(8/9))*(lead(pop,56)^(1/9)),0), 
        ifelse(year==1730, round((lag(pop,4)^(7/9))*(lead(pop,54)^(2/9)),0), 
        ifelse(year==1740, round((lag(pop,6)^(6/9))*(lead(pop,52)^(3/9)),0), 
        ifelse(year==1750, round((lag(pop,8)^(5/9))*(lead(pop,50)^(4/9)),0), 
        ifelse(year==1760, round((lag(pop,18)^(4/9))*(lead(pop,40)^(5/9)),0), 
        ifelse(year==1770, round((lag(pop,28)^(3/9))*(lead(pop,30)^(6/9)),0), 
        ifelse(year==1780, round((lag(pop,38)^(2/9))*(lead(pop,20)^(7/9)),0), 
        ifelse(year==1790, round((lag(pop,48)^(1/9))*(lead(pop,10)^(8/9)),0), 
               pop)))))))), 
               pop)
    ) %>% 
    mutate(
      pop=
        ifelse(year%in%seq(1715,1745,10), round((lag(pop,1)*lead(pop,1))^(1/2),0), 
               
        ifelse(year%in%seq(1751,1941,10), round((lag(pop,1)^(9/10))*(lead(pop,9)^(1/10)),0), 
        ifelse(year%in%seq(1752,1942,10), round((lag(pop,2)^(8/10))*(lead(pop,8)^(2/10)),0), 
        ifelse(year%in%seq(1753,1943,10), round((lag(pop,3)^(7/10))*(lead(pop,7)^(3/10)),0), 
        ifelse(year%in%seq(1754,1944,10), round((lag(pop,4)^(6/10))*(lead(pop,6)^(4/10)),0), 
        ifelse(year%in%seq(1755,1945,10), round((lag(pop,5)^(5/10))*(lead(pop,5)^(5/10)),0), 
        ifelse(year%in%seq(1756,1946,10), round((lag(pop,6)^(4/10))*(lead(pop,4)^(6/10)),0), 
        ifelse(year%in%seq(1757,1947,10), round((lag(pop,7)^(3/10))*(lead(pop,3)^(7/10)),0), 
        ifelse(year%in%seq(1758,1948,10), round((lag(pop,8)^(2/10))*(lead(pop,2)^(8/10)),0), 
        ifelse(year%in%seq(1759,1949,10), round((lag(pop,9)^(1/10))*(lead(pop,1)^(9/10)),0), 
               pop)))))))))),
    ) %>% 
    mutate(
      pop= # align Black Death population drop
        ifelse(analysis_region%in%c("Western Europe", "Middle Europe", "Northeast Europe", "North Africa", "West and Central Asia"), 
        ifelse(year==1345, round(lag(pop,9)*((lag(pop,9)/lag(pop,18))^(1/2)),0), 
        ifelse(year==1350, round((lag(pop,9)+lead(pop,9))/2,0), 
        ifelse(year==1355, round(lead(pop,9)*((lead(pop,9)/lead(pop,18))^(1/2)),0), 
               pop))),
               pop)
    ) %>% 
    mutate(
      pop=
        ifelse(analysis_region%in%c("Western Europe", "Middle Europe", "Northeast Europe", "North Africa", "West and Central Asia"), 
        ifelse(year%in%c(1305,1360), round(lag(pop,1)*(8/9)+lead(pop,8)*(1/9),0), 
        ifelse(year%in%c(1310,1365), round(lag(pop,2)*(7/9)+lead(pop,7)*(2/9),0), 
        ifelse(year%in%c(1315,1370), round(lag(pop,3)*(6/9)+lead(pop,6)*(3/9),0), 
        ifelse(year%in%c(1320,1375), round(lag(pop,4)*(5/9)+lead(pop,5)*(4/9),0), 
        ifelse(year%in%c(1325,1380), round(lag(pop,5)*(4/9)+lead(pop,4)*(5/9),0), 
        ifelse(year%in%c(1330,1385), round(lag(pop,6)*(3/9)+lead(pop,3)*(6/9),0), 
        ifelse(year%in%c(1335,1390), round(lag(pop,7)*(2/9)+lead(pop,2)*(7/9),0), 
        ifelse(year%in%c(1340,1395), round(lag(pop,8)*(1/9)+lead(pop,1)*(8/9),0), 
               pop)))))))),
               pop)
    ) %>% 
    mutate(
      pop= # align WWI population drop
        ifelse(analysis_region%in%c("Middle Europe", "Northeast Europe")|country_name%in%c("Germany", "France"), 
        ifelse(year==1913, round(lag(pop,3)*((lag(pop,3)/lag(pop,6))^(2/3)),0), 
        ifelse(year==1918, round(lead(pop,2)*((lead(pop,2)/lead(pop,4))^(2/3)),0), 
               pop)),
               pop)
    ) %>% 
    mutate(
      pop=
        ifelse(analysis_region%in%c("Middle Europe", "Northeast Europe")|country_name%in%c("Germany", "France"), 
        ifelse(year==1911, round(lag(pop,1)*(2/3)+lead(pop,2)*(1/3),0), 
        ifelse(year==1912, round(lag(pop,2)*(1/3)+lead(pop,1)*(2/3),0), 
                             
        ifelse(year==1919, round(lag(pop,1)*(1/2)+lead(pop,1)*(1/2),0), 
                                    
        ifelse(year==1914, round(lag(pop,1)*(4/5)+lead(pop,4)*(1/5),0), 
        ifelse(year==1915, round(lag(pop,2)*(3/5)+lead(pop,3)*(2/5),0), 
        ifelse(year==1916, round(lag(pop,3)*(2/5)+lead(pop,2)*(3/5),0), 
        ifelse(year==1917, round(lag(pop,4)*(1/5)+lead(pop,1)*(4/5),0), 
               pop))))))),
               pop)
    ) %>% 
    mutate(
      pop= # align WWII population drop
        ifelse(analysis_region%in%c("Middle Europe", "Northeast Europe")|country_name=="Germany", 
        ifelse(year==1945&pop<lag(pop,1), round(lead(pop,5)*((lead(pop,5)/lead(pop,10))^(2/3)),0), 
               pop),
               pop)
    ) %>% 
    mutate(
      pop=
        ifelse(analysis_region%in%c("Middle Europe", "Northeast Europe")|country_name=="Germany", 
        ifelse(year%in%c(1941,1946)&pop>lead(pop,1), round(lag(pop,1)*(4/5)+lead(pop,4)*(1/5),0), 
        ifelse(year%in%c(1942,1947)&pop>lead(pop,1), round(lag(pop,2)*(3/5)+lead(pop,3)*(2/5),0), 
        ifelse(year%in%c(1943,1948)&pop>lead(pop,1), round(lag(pop,3)*(2/5)+lead(pop,2)*(3/5),0), 
        ifelse(year%in%c(1944,1949)&pop>lead(pop,1), round(lag(pop,4)*(1/5)+lead(pop,1)*(4/5),0), 
               pop)))),
               pop)
    ) %>% 
    mutate(
      pop= # align post-Columbus population drop
        ifelse(continent%in%c("North America", "South America")&sub_region!="Caribbean", 
        ifelse(year==1520&pop<lag(pop,1), round(lag(pop,4)*((lag(pop,4)/lag(pop,8))^(1/2)),0), 
        ifelse(year%in%seq(1580,1595,5)&pop<lag(pop,1), round(lead(pop,4),0), 
               pop)),
               pop)
    ) %>% 
    mutate(
      pop=
        ifelse(continent%in%c("North America", "South America")&sub_region!="Caribbean", 
        ifelse(year==1505&pop>lead(pop,5), round(lag(pop,1)*(3/4)+lead(pop,3)*(1/4),0), 
        ifelse(year==1510&pop>lead(pop,5), round(lag(pop,2)*(2/4)+lead(pop,2)*(2/4),0), 
        ifelse(year==1515&pop>lead(pop,5), round(lag(pop,3)*(1/4)+lead(pop,1)*(3/4),0), 
                                    
        ifelse(year==1525&pop>lead(pop,1), round(lag(pop,1)*(11/12)+lead(pop,11)*(1/12),0), 
        ifelse(year==1530&pop>lead(pop,1), round(lag(pop,2)*(10/12)+lead(pop,10)*(2/12),0), 
        ifelse(year==1535&pop>lead(pop,1), round(lag(pop,3)*(9/12)+lead(pop,9)*(3/12),0), 
        ifelse(year==1540&pop>lead(pop,1), round(lag(pop,4)*(8/12)+lead(pop,8)*(4/12),0), 
        ifelse(year==1545&pop>lead(pop,1), round(lag(pop,5)*(7/12)+lead(pop,7)*(5/12),0), 
        ifelse(year==1550&pop>lead(pop,1), round(lag(pop,6)*(6/12)+lead(pop,6)*(6/12),0), 
        ifelse(year==1555&pop>lead(pop,1), round(lag(pop,7)*(5/12)+lead(pop,5)*(7/12),0), 
        ifelse(year==1560&pop>lead(pop,1), round(lag(pop,8)*(4/12)+lead(pop,4)*(8/12),0), 
        ifelse(year==1565&pop>lead(pop,1), round(lag(pop,9)*(3/12)+lead(pop,3)*(9/12),0), 
        ifelse(year==1570&pop>lead(pop,1), round(lag(pop,10)*(2/12)+lead(pop,2)*(10/12),0), 
        ifelse(year==1575&pop>lead(pop,1), round(lag(pop,11)*(1/12)+lead(pop,1)*(11/12),0), 
               pop)))))))))))))),
               pop)
    ) %>% 
    ungroup()
  
  return(object1)
}
