#######################################################################
#
# Freight Data Cleansing and Analysis
#
# started: 2019-08-17
# revised: 2019-10-26
# Team 6
#
#######################################################################


# Load Relevant packages
library(tidyverse)
library(data.table)
library(lubridate)
library(ggplot2)

#set working director if necessary
#setwd()

# Load datasets
freight1 <- fread('freight_sheet1.csv')
freight2 <- fread('freight_sheet2.csv')

# bind data
freightdata <- rbind(freight1,freight2)

# set appropriate data types to each variable
freightdata <- freightdata %>% 
  mutate(UNITNUMBER=as.factor(UNITNUMBER),
         TRUCKSTATUS=as.factor(TRUCKSTATUS),
         TYPE=as.factor(TYPE),
         MAKE=as.factor(MAKE),
         MODEL=as.factor(MODEL),
         ACTIVCODE=as.factor(ACTIVCODE),
         TICKETSTATUS=as.factor(TICKETSTATUS),
         ENGINE=as.factor(ENGINE),
         ORDERID=as.factor(ORDERID),
         ORDERNUM=as.factor(ORDERNUM),
         SHOPID=as.factor(SHOPID),
         VENDOR=as.factor(VENDOR),
         CREATEDBY=as.factor(CREATEDBY),
         SECTIONNUM=as.factor(SECTIONNUM),
         SECSTATUS=as.factor(SECSTATUS),
         REPREASON=as.factor(REPREASON),
         COMPCODE=as.factor(COMPCODE),
         SYSTEM=as.factor(SYSTEM),
         COMP_DESCRIPTION=as.factor(COMP_DESCRIPTION),
         COMPLAINT=as.factor(COMPLAINT),
         LINETYPE=as.factor(LINETYPE),
         Total=as.numeric(Total),
         QTYREQD=as.numeric(QTYREQD),
         HOURS=as.numeric(HOURS),
         INSERVICE=as_date(mdy(INSERVICE)),
         RO_OPENED=as_date(mdy_hm(RO_OPENED)),
         RO_COMPLTDATE=as_date(mdy_hm(RO_COMPLTDATE)),
         CLOSED=as_date(mdy_hm(CLOSED)),
         CREATEDON=as_date(mdy_hm(CREATEDON)))

# Remove Owner Operator trucks as these tickets do not match all tickets
freightdata <- freightdata %>%
  filter(TRUCKSTATUS!="OWNER OPER")

# change Total to 0 if part number is null or n/a
freightdata <- freightdata %>%
  mutate(Total = case_when(
    PARTNUMBER == "NULL" | is.na(PARTNUMBER) ~ 0,
    TRUE ~ Total))

# change QtyReqd to 0 if part number is null or n/a
freightdata <- freightdata %>%
  mutate(QTYREQD = case_when(
    PARTNUMBER == "NULL" | is.na(PARTNUMBER) ~ 0,
    TRUE ~ QTYREQD))

# change NULL/NA hours to zero
freightdata <- freightdata %>%
  mutate(HOURS = case_when(
    HOURS == "NULL" | is.na(HOURS) ~ 0,
    TRUE ~ HOURS))

# remove open tickets
freightdata <- freightdata %>%
  filter(TICKETSTATUS=="CLOSED" | TICKETSTATUS=="COMPLETE")

# remove observations where tractor make and engine type are null
freightdata <- freightdata %>%
  mutate(index=if_else(TYPE=="TRACTOR"&ENGINE=="",1,0),
         index2=if_else(TYPE=="TRACTOR"&ENGINE=="NULL",1,0),
         counter=index+index2)
freightdata <- freightdata %>%
  filter(counter==0) %>%
  select(-counter, -index,-index2)

# Some good ol' cleaning for SYSTEM, replacing missing values
freightdata <- freightdata %>% 
  mutate(SYSTEM=str_replace(SYSTEM, "999", ""),
         SYSTEM=str_replace(SYSTEM, "FNA", ""),
         SYSTEM=as.factor(SYSTEM))

# remove the 2 remaining RO_COMPLTDATE NAs
#freightdata <- freightdata %>%
#  filter(RO_COMPLTDATE!=is.na(RO_COMPLTDATE))

# Impute NA closed values with the corresponding RO_COMPLTDATE
freightdata <- freightdata %>%
  dplyr::mutate(CLOSED = case_when(
    is.na(CLOSED) ~ RO_COMPLTDATE,
    TRUE ~ CLOSED
  ))

# this code will overwrite tickets that include breakdown in addition to other repreasons with BREAKDOWN
freightdata <- freightdata %>%
  mutate(REPREASON=str_replace(REPREASON, "ROADCALL", "BREAKDOWN"))


breakdown <- freightdata %>% 
  filter(REPREASON=="BREAKDOWN") %>%
  mutate(repreasonreference=REPREASON,
         orderidreference=ORDERID) %>% 
  distinct(ORDERID,.keep_all=TRUE) %>% 
  select(repreasonreference,ORDERID,orderidreference)

freightdata <- left_join(freightdata,breakdown,by="ORDERID")

freightdata <- freightdata %>%
  mutate(REPREASON = case_when(
    ORDERID == orderidreference ~ repreasonreference,
    TRUE ~ REPREASON)) %>% 
  select(-repreasonreference,-orderidreference)

freightdata <- freightdata %>% 
  mutate(BREAKDOWN=as.factor(ifelse(REPREASON=="BREAKDOWN",1,0)))

# String Replace PM - Follow Up to PM
freightdata <- freightdata %>%
  mutate(REPREASON=str_replace(REPREASON, "PM FOLLOW-UP", "PM")) 

# Replace FLAT TIRE CODE

freightdata <- freightdata %>% 
  mutate(REPREASON=str_replace(REPREASON, "BREAKDOWN-TIR", "FLAT"),
         REPREASON=str_replace(REPREASON, "TIRE", "FLAT"))

flat <- freightdata %>% 
  filter(REPREASON=="FLAT") %>%
  mutate(repreasonreference=REPREASON,
         orderidreference=ORDERID) %>% 
  distinct(ORDERID,.keep_all=TRUE) %>% 
  select(repreasonreference,ORDERID,orderidreference)

freightdata <- left_join(freightdata,flat,by="ORDERID")

freightdata <- freightdata %>%
  mutate(REPREASON = case_when(
    ORDERID == orderidreference ~ repreasonreference,
    TRUE ~ REPREASON)) %>% 
  select(-repreasonreference,-orderidreference)

# Replace REPREASON for items that have COMPLAINT of "NEW UNIT"
freightdata <- freightdata %>%
  mutate(COMPLAINT=as.character(COMPLAINT),
         REPREASON=as.character(REPREASON)) %>% 
  mutate(REPREASON=case_when(
    COMPLAINT == "NEW UNIT" ~ "PM",
    TRUE ~ REPREASON)) %>% 
  mutate(COMPLAINT=as.factor(COMPLAINT),
         REPREASON=as.factor(REPREASON))

# this code will overwrite tickets that include PM in addition to other repreasons with PM
PM <- freightdata %>% 
  filter(REPREASON=="PM") %>%
  mutate(repreasonreference=REPREASON,
         orderidreference=ORDERID) %>% 
  distinct(ORDERID,.keep_all=TRUE) %>% 
  select(repreasonreference,ORDERID,orderidreference)

freightdata <- left_join(freightdata,PM,by="ORDERID")

freightdata <- freightdata %>%
  mutate(REPREASON = case_when(
    ORDERID == orderidreference ~ repreasonreference,
    TRUE ~ REPREASON)) %>% 
  dplyr::select(-repreasonreference,-orderidreference)

freightdata <- freightdata %>%
  mutate(REPREASON=as.factor(REPREASON))

# this code will overwrite tickets that include INSPECTION in addition to other repreasons with INSPECTION
inspection <- freightdata %>% 
  filter(REPREASON=="INSPECTION") %>%
  mutate(repreasonreference=REPREASON,
         orderidreference=ORDERID) %>% 
  distinct(ORDERID,.keep_all=TRUE) %>% 
  select(repreasonreference,ORDERID,orderidreference)

freightdata <- left_join(freightdata,inspection,by="ORDERID")

freightdata <- freightdata %>%
  mutate(REPREASON = case_when(
    ORDERID == orderidreference ~ repreasonreference,
    TRUE ~ REPREASON)) %>% 
  select(-repreasonreference,-orderidreference)

freightdata <- freightdata %>%
  mutate(REPREASON=as.factor(REPREASON))

#summary(freightdata$REPREASON)

# Roll up related REPREASONS to NO CONTROL
freightdata <- freightdata %>%
  mutate(REPREASON=str_replace(REPREASON, "ACTOFGOD", "NOCONTROL")) %>% 
  mutate(REPREASON=str_replace(REPREASON, "THEFT", "NOCONTROL")) %>% 
  mutate(REPREASON=str_replace(REPREASON, "VANDALISM", "NOCONTROL")) %>% 
  mutate(REPREASON=as.factor(REPREASON))

nocontrol <- freightdata %>% 
  filter(REPREASON=="NOCONTROL") %>%
  mutate(repreasonreference=REPREASON,
         orderidreference=ORDERID) %>% 
  distinct(ORDERID,.keep_all=TRUE) %>% 
  select(repreasonreference,ORDERID,orderidreference)

freightdata <- left_join(freightdata,nocontrol,by="ORDERID")

freightdata <- freightdata %>%
  mutate(REPREASON = case_when(
    ORDERID == orderidreference ~ repreasonreference,
    TRUE ~ REPREASON)) %>% 
  select(-repreasonreference,-orderidreference)

freightdata <- freightdata %>%
  mutate(REPREASON=as.factor(REPREASON))

# Roll up related REPREASONS for ACCIDENTS
freightdata <- freightdata %>%
  mutate(REPREASON=str_replace(REPREASON, "ACC-NON-REPO", "ACCIDENT")) %>% 
  mutate(REPREASON=str_replace(REPREASON, "ACC-REPORT", "ACCIDENT")) %>% 
  mutate(REPREASON=as.factor(REPREASON))

accident <- freightdata %>% 
  filter(REPREASON=="ACCIDENT") %>%
  mutate(repreasonreference=REPREASON,
         orderidreference=ORDERID) %>% 
  distinct(ORDERID,.keep_all=TRUE) %>% 
  select(repreasonreference,ORDERID,orderidreference)

freightdata <- left_join(freightdata,accident,by="ORDERID")

freightdata <- freightdata %>%
  mutate(REPREASON = case_when(
    ORDERID == orderidreference ~ repreasonreference,
    TRUE ~ REPREASON)) %>% 
  select(-repreasonreference,-orderidreference)

freightdata <- freightdata %>%
  mutate(REPREASON=as.factor(REPREASON))


# Roll up related REPREASONS for General Maintenance
freightdata <- freightdata %>%
  mutate(REPREASON=str_replace(REPREASON, "DRIVERREPORT", "MAINTENANCE")) %>% 
  mutate(REPREASON=str_replace(REPREASON, "FUELCONSUMPT", "MAINTENANCE")) %>% 
  mutate(REPREASON=str_replace(REPREASON, "IMPROVEMENT", "MAINTENANCE")) %>% 
  mutate(REPREASON=str_replace(REPREASON, "LUBRICATION", "MAINTENANCE")) %>% 
  mutate(REPREASON=str_replace(REPREASON, "MODIFICATION", "MAINTENANCE")) %>% 
  mutate(REPREASON=str_replace(REPREASON, "OILCONSUMPT", "MAINTENANCE")) %>% 
  mutate(REPREASON=str_replace(REPREASON, "TIRE", "MAINTENANCE")) %>% 
  mutate(REPREASON=str_replace(REPREASON, "INSVC", "MAINTENANCE")) %>%
  mutate(REPREASON=str_replace(REPREASON, "REWORK", "MAINTENANCE")) %>% 
  mutate(REPREASON=as.factor(REPREASON))

MAINTENANCE <- freightdata %>% 
  filter(REPREASON=="MAINTENANCE") %>%
  mutate(repreasonreference=REPREASON,
         orderidreference=ORDERID) %>% 
  distinct(ORDERID,.keep_all=TRUE) %>% 
  select(repreasonreference,ORDERID,orderidreference)

freightdata <- left_join(freightdata,MAINTENANCE,by="ORDERID")

freightdata <- freightdata %>%
  mutate(REPREASON = case_when(
    ORDERID == orderidreference ~ repreasonreference,
    TRUE ~ REPREASON)) %>% 
  select(-repreasonreference,-orderidreference)

freightdata <- freightdata %>%
  mutate(REPREASON=as.factor(REPREASON))

# filter out OWNER OP TRKs
freightdata <- freightdata %>% 
  filter(REPREASON != "OWNER OP TRK")

# Roll up related REPREASONS for Warranty
freightdata <- freightdata %>%
  mutate(REPREASON=str_replace(REPREASON, "RECALL", "WARRANTY")) %>% 
  mutate(REPREASON=as.factor(REPREASON))

warranty <- freightdata %>% 
  filter(REPREASON=="WARRANTY") %>%
  mutate(repreasonreference=REPREASON,
         orderidreference=ORDERID) %>% 
  distinct(ORDERID,.keep_all=TRUE) %>% 
  select(repreasonreference,ORDERID,orderidreference)

freightdata <- left_join(freightdata,warranty,by="ORDERID")

freightdata <- freightdata %>%
  mutate(REPREASON = case_when(
    ORDERID == orderidreference ~ repreasonreference,
    TRUE ~ REPREASON)) %>% 
  select(-repreasonreference,-orderidreference)

freightdata <- freightdata %>%
  mutate(REPREASON=as.factor(REPREASON))

# Remaining REPREASONS rolled into MAINTENANCE
freightdata <- freightdata %>%
  mutate(REPREASON=str_replace(REPREASON, "OPS EXP", "MAINTENANCE")) %>% 
  mutate(REPREASON=str_replace(REPREASON, "OUTSVC", "MAINTENANCE")) %>% 
  mutate(REPREASON=as.factor(REPREASON))

damage <- freightdata %>% 
  filter(REPREASON=="DAMAGE") %>%
  mutate(repreasonreference=REPREASON,
         orderidreference=ORDERID) %>% 
  distinct(ORDERID,.keep_all=TRUE) %>% 
  select(repreasonreference,ORDERID,orderidreference)

freightdata <- left_join(freightdata,damage,by="ORDERID")

freightdata <- freightdata %>%
  mutate(REPREASON = case_when(
    ORDERID == orderidreference ~ repreasonreference,
    TRUE ~ REPREASON)) %>% 
  select(-repreasonreference,-orderidreference)

freightdata <- freightdata %>%
  mutate(REPREASON=as.factor(REPREASON))

# CREATE COUNT COLUMNS FOR EVERY REPREASON
freightdata <- freightdata %>% 
  mutate(ACCIDENT=as.numeric(ifelse(REPREASON=="ACCIDENT",1,0)),
         DAMAGE=as.numeric(ifelse(REPREASON=="DAMAGE",1,0)),
         INSPECTION=as.numeric(ifelse(REPREASON=="INSPECTION",1,0)),
         MAINTENANCE=as.numeric(ifelse(REPREASON=="MAINTENANCE",1,0)),
         NOCONTROL=as.numeric(ifelse(REPREASON=="NOCONTROL",1,0)),
         PM=as.numeric(ifelse(REPREASON=="PM",1,0)),
         WARRANTY=as.numeric(ifelse(REPREASON=="WARRANTY",1,0)),
         BREAKDOWNCOUNT=as.numeric(ifelse(REPREASON=="BREAKDOWN",1,0)),
         FLAT=as.numeric(ifelse(REPREASON=="FLAT",1,0)))

freightdata <- freightdata %>% 
  mutate(SYSTEM=str_replace(SYSTEM,"^0$",""),
         SYSTEM=str_replace(SYSTEM,"^([1-4])$","CAB RELATED"),
         SYSTEM=str_replace(SYSTEM,"^([1][0-9])$","CHASSIS RELATED"),
         SYSTEM=str_replace(SYSTEM,"^([2][0-9])$","DRIVETRAIN RELATED"),
         SYSTEM=str_replace(SYSTEM,"^([3][0-9])$","ELECTRICAL RELATED"),
         SYSTEM=str_replace(SYSTEM,"^([4][0-9])$","ENGINE RELATED"),
         SYSTEM=str_replace(SYSTEM,"^([5][0-9])$","ACCESSORIES RELATED"),
         SYSTEM=str_replace(SYSTEM,"^([6][0-9])$","EQUIPMENT ATTACHMENT RELATED"),
         SYSTEM=str_replace(SYSTEM,"^([7][0-9])$","BODY RELATED"),
         SYSTEM=str_replace(SYSTEM,"^150$","ELECTRICAL RELATED"),
         SYSTEM=str_replace(SYSTEM,"^151$","ACCESSORIES RELATED"),
         SYSTEM=str_replace(SYSTEM,"^153$","ELECTRICAL RELATED"),
         SYSTEM=str_replace(SYSTEM,"^162$","ELECTRICAL RELATED"),
         SYSTEM=str_replace(SYSTEM,"^168$","ACCESSORIES RELATED"),
         SYSTEM=str_replace(SYSTEM,"^174$","BODY RELATED"),
         SYSTEM=str_replace(SYSTEM,"^266$","BODY RELATED"),
         SYSTEM=str_replace(SYSTEM,"^361$","BODY RELATED"),
         SYSTEM=str_replace(SYSTEM,"^392$","ACCESSORIES RELATED"),
         SYSTEM=as.factor(SYSTEM)) %>% 
  arrange(SYSTEM)

# GROUP BY to reduce each ticket to 1 observation
freightgroup <- freightdata %>%
  select(-PARTNUMBER,-TRUCKSTATUS,-DESCRIPTION,-INSERVICE,-MONTHS_OLD,-SHOPID,-ORDERTYPE,-ORDERNUM,-TICKETSTATUS,-VENDOR,-CREATEDON,
         -CREATEDBY,-CLOSEDBY,-SECSTATUS,-COMPCODE,-COMP_DESCRIPTION,-LINETYPE,-ClaimNumber,-QTYRCVD,-QTYREQD,
         -CLOSED,-SECTIONNUM,-COMPLAINT) %>% 
  dplyr::group_by(ORDERID,UNITNUMBER,BREAKDOWN,MODELYEAR,ACTIVCODE,MAKE,MODEL,ENGINE,TYPE,DAYS_OLD,
                  RO_OPENED,RO_COMPLTDATE,REPREASON,ACCIDENT,DAMAGE,INSPECTION,MAINTENANCE,NOCONTROL,PM,WARRANTY,BREAKDOWNCOUNT,FLAT) %>%
  summarise(Total=sum(Total),HOURS=sum(HOURS),SYSTEM = paste(unique(SYSTEM), collapse=", "))

# remove the 2 remaining RO_COMPLTDATE NAs from freightgroup
freightgroup <- freightgroup %>%
  filter(RO_COMPLTDATE!=is.na(RO_COMPLTDATE))

# remove the 2 remaining RO_COMPLTDATE NAs from freightdata
freightdata <- freightdata %>%
  filter(RO_COMPLTDATE!=is.na(RO_COMPLTDATE))

# Create a column that displays length of time between tickets/breakdowns
freight_tdiff <- freightgroup %>% 
  ungroup() %>% 
  dplyr::select(BREAKDOWN,ORDERID,UNITNUMBER,RO_OPENED)

# From stackoverflow: https://stackoverflow.com/questions/58582784/i-am-trying-to-create-features-that-determine-time-between-events-based-off-of-c
freight_tdiff <- freight_tdiff %>% 
  arrange(UNITNUMBER, RO_OPENED) %>% 
  group_by(UNITNUMBER, BREAKDOWN) %>% 
  mutate(TDIFF = coalesce(RO_OPENED - lag(RO_OPENED), 0),
         TDIFF = ifelse(BREAKDOWN == 0, NA, TDIFF),
         Number = ifelse(BREAKDOWN == 0, NA, seq_along(RO_OPENED))) %>% 
  ungroup()


# Join freight_tdiff and freightgroup
freight_tdiff <- freight_tdiff %>% 
  dplyr::select(ORDERID,UNITNUMBER,TDIFF,Number)
freightnew <- left_join(freightgroup,freight_tdiff,by=c("ORDERID","UNITNUMBER"))
freightnew <- freightnew %>%
  ungroup %>% 
  mutate(SYSTEM=as.factor(SYSTEM))

# Roll up all reps so that we have 1 row per truck
freightforest <- freightnew %>% 
  ungroup() %>% 
  select(-REPREASON,-BREAKDOWN,-TDIFF,-DAYS_OLD,-RO_OPENED,-RO_COMPLTDATE,-ORDERID,-DAYS_OLD) %>% 
  arrange(SYSTEM) %>% 
  dplyr::group_by(UNITNUMBER,MODELYEAR,ACTIVCODE,MAKE,MODEL,ENGINE,TYPE) %>%
  summarise(SYSTEM = paste(unique(SYSTEM), collapse=", "),Total=sum(Total),HOURS=sum(HOURS),ACCIDENT=sum(ACCIDENT),DAMAGE=sum(DAMAGE),INSPECTION=sum(INSPECTION),
            MAINTENANCE=sum(MAINTENANCE),NOCONTROL=sum(NOCONTROL),PM=sum(PM),WARRANTY=sum(WARRANTY),
            BREAKDOWN=sum(BREAKDOWNCOUNT),FLAT=sum(FLAT))

freightforest <- freightforest %>% 
  ungroup %>% 
  mutate(SYSTEM=as.factor(SYSTEM))

freightnew_system <- freightnew %>%
  mutate(SYSTEM=str_replace(SYSTEM,"^,",""),
         SYSTEM=str_replace(SYSTEM,"^ ,","")) %>% 
  mutate(SYSTEM=as.factor(SYSTEM))



summary(freightnew_system)

# Write to csvs for both datasets 
write.csv(freightnew,file="freightnew.csv")
write.csv(freightforest,file="freightforest.csv")
write.csv(freightnew_system,file="freightnew_system.csv")

freightdata %>% select(ORDERID,SYSTEM) %>% write.csv(file='freight_System2.csv')
