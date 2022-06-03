setwd("G:/CE-CERT/CEMDAP/Riv_data/")
## load data
employment = read.csv('employment.csv')
node = read.csv('Riverside_time_coord_updat.csv')
schedule = read.csv("eatout_trips.csv")

library(geosphere)
library(dplyr)

############## first part, match restaurants to nearby node
riv_emp_food = employment %>% filter(City == "Riverside", Industry %in% c('Full-service restaurants', 'Limited-service restaurants')) %>% select(Longitude,Latitude) 
#write.csv(riv_emp_food,'riv_emp_food.csv',row.names = F)
schedule_food = schedule %>% filter(acType == 3,startT>690, startT<750)
node = node %>% select(from_Lon, from_Lat)
node = node[!duplicated(node),]
start_time = Sys.time()
distance_matrix = distm(node,riv_emp_food,fun = distGeo)
end_time = Sys.time()
round(end_time-start_time)

min_index = apply(distance_matrix, 2, which.min)
riv_emp_food = node[min_index,]
#write.csv(node[min_index,],'restaurants.csv', row.names = F)


# map start to node
start_time = Sys.time()
distance_matrix_start = distm(node,schedule_food[,c(14,13)],fun = distGeo)
end_time = Sys.time()
round(end_time-start_time)
min_index = apply(distance_matrix_start, 2, which.min)
schedule_food[,c(14,13)] = node[min_index,]

# map end to restaurants
start_time = Sys.time()
distance_matrix_end = distm(riv_emp_food,schedule_food[,c(16,15)],fun = distGeo)
end_time = Sys.time()
round(end_time-start_time)
min_index = apply(distance_matrix_end, 2, which.min)
schedule_food[,c(16,15)] = riv_emp_food[min_index,]

## find long distance
start_time = Sys.time()
for (i in 1:dim(schedule_food)[1]) {
  var1 = c(schedule_food$origin_x[i],schedule_food$origin_y[i])
  var2 = c(schedule_food$dest_x[i],schedule_food$dest_y[i])
  d = distm(var1,var2)
  if(d > 15000){
     di = distm(riv_emp_food,var1,fun = distGeo)
     min_index = which(di <15000)
     if(length(min_index)==0){
       schedule_food$dest_x[i] = schedule_food$origin_x[i]
       schedule_food$dest_y[i] = schedule_food$origin_y[i]
     }else{
       min_index = sample(min_index,1)
       schedule_food$dest_x[i] = riv_emp_food[min_index,1]
       schedule_food$dest_y[i] = riv_emp_food[min_index,2]
     }
  }
  end_time = Sys.time()
  print(c(i, round(end_time-start_time,5)))
}                                     
setwd("G:/CE-CERT/CEMDAP/Riv_data/")
write.csv(schedule_food,'short_trip_filtered_8_yejia.csv',row.names = F)
