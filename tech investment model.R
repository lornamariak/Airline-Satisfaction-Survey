#Modeling the tech investment question: Technology investment to improve wifi and entertainment service(updates wifi and enetr)
library(dplyr)
techdf <-
  airline %>% dplyr::select(
    c(
      Satisfaction,
      Age,
      Gender,
      Type.of.Travel,
      Class,
      Flight.Distance,
      Inflight.wifi.service,
      Inflight.entertainment,
      Online.boarding,
      Ease.of.Online.booking
    )
  )
#Wifi
techdf1 <-
  techdf %>% mutate(Inflight.wifi.service = na_if(Inflight.wifi.service,0),
    wifi.service = case_when(
      Inflight.wifi.service == 1 ~ "Not satisfied",
      Inflight.wifi.service == 2 ~ "Partly Satisfied",
      Inflight.wifi.service == 3 ~ "Satisfied",
      Inflight.wifi.service == 4 ~ "More than Satisfied",
      Inflight.wifi.service == 5 ~ "Very Satisfied"
    )
  )
#Entertainment
techdf2 <-
  techdf1 %>% mutate(Inflight.entertainment= na_if(Inflight.entertainment,0),
    entertainment = case_when(
      Inflight.entertainment == 1 ~ "Not satisfied",
      Inflight.entertainment == 2 ~ "Partly Satisfied",
      Inflight.entertainment == 3 ~ "Satisfied",
      Inflight.entertainment == 4 ~ "More than Satisfied",
      Inflight.entertainment == 5 ~ "Very Satisfied"
    )
  )
#Booking
techdf3 <-
  techdf2 %>% mutate(Online.boarding = na_if(Online.boarding,0),
    boarding = case_when(
      Online.boarding == 1 ~ "Not satisfied",
      Online.boarding == 2 ~ "Partly Satisfied",
      Online.boarding == 3 ~ "Satisfied",
      Online.boarding == 4 ~ "More than Satisfied",
      Online.boarding == 5 ~ "Very Satisfied"
    )
  )
#Boarding
techdf4 <-
  techdf3 %>% mutate(Ease.of.Online.booking = na_if(Ease.of.Online.booking,0),
    booking = case_when(
      Ease.of.Online.booking == 1 ~ "Not satisfied",
      Ease.of.Online.booking == 2 ~ "Partly Satisfied",
      Ease.of.Online.booking == 3 ~ "Satisfied",
      Ease.of.Online.booking == 4 ~ "More than Satisfied",
      Ease.of.Online.booking == 5 ~ "Very Satisfied"
    )
  )

#check if transformation was done well
#assertthat::are_equal(nrow(techdf4), nrow(techdf))

#clean df: drop the others
finaltechdf = techdf4 %>% select(
                      -c(
                        Inflight.wifi.service,
                        Inflight.entertainment,
                        Online.boarding,
                        Ease.of.Online.booking
                      )
)

#fix col types
for (i in colnames(finaltechdf)){
  print(typeof(finaltechdf[[i]]))
}

for (i in 1:ncol(finaltechdf)){
  if(is.character(finaltechdf[,i])){
    finaltechdf[,i]=factor(finaltechdf[,i])
  }
}
str(finaltechdf)



#model 
library(MASS)

model1 <- polr(Satisfaction ~ Age +
               Gender+
               Type.of.Travel +
               Class +
               Flight.Distance +
               wifi.service +
               entertainment +
               boarding +
               booking, data = finaltechdf, Hess = TRUE)
summary(model1)

#p values 
p_values <- pnorm(-abs(summary(model1)$coef[,"t value"]))*2
ptable <- cbind(summary(model1)$coef, p_values)
ptable

  