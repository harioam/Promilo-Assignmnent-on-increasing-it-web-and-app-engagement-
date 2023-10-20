setwd("C:/Users/hario/Downloads")
getwd()
library(readxl)
data=read_xlsx("C:/Users/hario/Downloads/App Analytics Report-06.05.2023.xlsx")
View(data)
df1=read_xlsx("C:/Users/hario/Downloads/App Analytics Report-06.05.2023.xlsx",sheet = 3)
View(df1)
df2= read_xlsx("C:/Users/hario/Downloads/App Analytics Report-06.05.2023.xlsx",sheet = 4)             
View(df2)
View(df1$`Total revenue`)
df3= read_xlsx("C:/Users/hario/Downloads/App Analytics Report-06.05.2023.xlsx",sheet = 5) 
View(df3)
library(ggplot2)
# Create a bar plot
ggplot(data=df1, aes(x =`First user default channel group` , y =`New users`)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "New users by First User Default Channel Group", x = "Channel Group", y = "New Users") +
  theme_minimal()
ggplot(data = df1, aes(x = `First user default channel group`, y = `New users`)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "New Users by First User Default Channel Group", x = "Channel Group", y = "New Users") +
  theme_minimal()
library(reshape2)
# Load required packages
library(ggplot2)
library(reshape2)

# Sample data frame
data <- data.frame(
  `First user default channel group` = c("Direct", "Display", "Organic Search", "Organic Social", "Paid Search", "Unassigned"),
  `NewUsers` = c(1903, 9957, 7652, 10, 3025, 325),
  `EngagementRate` = c(0.318808, 0.544457, 0.813680, 0.722222, 0.474284, 0.813159)
  ``
)

# Reshape the data
melted_data <- melt(df1, id.vars = "First user default channel group")

# Create the heatmap
ggplot(melted_data, aes(x = c(), y = `First user default channel group`, fill = value*100)) +
  geom_tile() +
  scale_fill_gradient(low = "orange", high = "red") +
  labs(title = "Heatmap of Metrics by Channel Group", x = "Metrics", y = "Channel Groups") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####

library(ggplot2)

# Sample data frame with corrected column names


# Reshape the data
melted_data <- melt(df2, id.vars = "First user default channel group")

# Create a grouped bar chart for Conversions, New Users, Engaged Sessions, and Event Count
ggplot(melted_data, aes(x = `First user default channel group`, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Metrics by Channel Group", x = "First user default channel group", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~variable, scales = "free_y", ncol = 2)

###now we will analyse the traffic data report of user 
melteddata1=melt(df2,id.vars = "Session default channel group")
####to create a grouped bar chart  for users, sessions,engagement rate and conversion.
ggplot(melteddata1,aes(x=`Session default channel group`,y=value,fill=variable))
+geom_bar(stat = "identity",position = "dodge")+
  labs(title = "sessions from user traffic",x=`Session default channel group`,y="value")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~variable, scales = "free_y", ncol = 3)

ggplot(melteddata1, aes(x = `Session default channel group`, y =value, fill =variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Metrics by Session Default Channel Group", x = "session default Channel Group", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~variable, scales = "free_y", ncol = 3)
####now we move on to event count
library(dplyr)
df3=df3 %>%
  group_by(`Event name`) %>%
  summarise(`Event count`=sum(`Event count`)) %>%
  top_n(5,`Event count`) %>%
  arrange(desc(`Event count`))


ggplot(df3, aes(x = `Event name`, y = `Event count`)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Event Count by Event Name", x = "Event Name", y = "Event Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####for coversion report 
df4= read_xlsx("C:/Users/hario/Downloads/App Analytics Report-06.05.2023.xlsx",sheet = 6) 
df4  
View(df4)
df4_1= df4 %>%
  group_by(`Event name`) %>%
  summarise(Conversions=sum(Conversions)) %>%
  top_n(3,`Event name`) %>%
  arrange(desc(`Event name`))

ggplot(df4_1,aes(x=`Event name`,y= Conversions))+geom_bar(stat = "identity",fill="orange")+
  labs(title = "Conversion as per Event Name",x = "Event name" , y = "Conversion")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 30 , hjust = 1))
#####
p <- ggplot(data, aes(x = Category)) +
  geom_bar(aes(y = BarData), stat = "identity", fill = "skyblue") +  # Bar chart
  geom_line(aes(y = LineData, group = 1), color = "red", position = "dodge", size = 1) +  # Line chart on top of bars
  scale_y_continuous(name = "Bar Data", sec.axis = sec_axis(trans = ~ . / 10, name = "Line Data")) +  # Secondary axis for the line chart
  labs(title = "Dual-Axis Chart with Line on Bars", x = "Category") +
  theme_minimal()

##### NOW WE WIIL ANAYLSE REPORT DEMOGRAPHIC WISW
df8_1=  read_xlsx("C:/Users/hario/Downloads/App Analytics Report-06.05.2023.xlsx",sheet = 10)   
print(df8)

df8=df8_1 %>%
  top_n(5,Users)

ggplot(df8_1 , aes(x=country )) +
  geom_bar(aes(y= `New users`) , stat = "identity" , fill = "skyblue") +
  geom_line(aes(y=`Engaged sessions` ) , color = "red" , position = "dodge" , size = 1) +
  geom_line(aes(y= "Conversions" ) , color = "blue", position = "dodge" , size =1 ) +
  scale_y_continuous(name = "New Users", sec.axis = sec_axis( name = "Engaged sessions and conversions")) +  # Secondary axis for the line charts
  labs(title = "Demographic users and engagement rate", x = "Country") +
  theme_minimal()


library(ggplot2)

# Sample data
data <- data.frame(
  "Country" = c("India", "United States", "Canada", "NOT SET", "United Kingdom", "Singapore", "Japan"),
  "New users" = c(22528, 213, 18, 36, 8, 6, 6),
  "Engaged sessions" = c(41479, 197, 25, 17, 13, 13, 11),
  "Conversions" = c(192766, 643, 121, 54, 43, 29, 24)
)

# Create a bar plot for New users, Engaged sessions, and Conversions

df8= df8[df8$Country != "(not set)", ]
p <- ggplot(df8, aes(x = Country)) +
  geom_bar(aes(y = `New users`, fill = "New users"), stat = "identity", position = "dodge") +
  geom_line(aes(y = `Engaged sessions`,group = 1, color = "Engaged sessions"), size = 1) +
  geom_line(aes(y = `Conversions`, group = 2 ,color = "Conversions"), size = 1, linetype = "dashed") +
  scale_fill_manual(values = "blue") +
  scale_color_manual(values = c("Engaged sessions" = "green", "Conversions" = "red")) +
  labs(title = "New Users, Engaged Sessions, and Conversions by Country", x = "Country", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p)

#### by age and gender
df9 = read_xlsx("C:/Users/hario/Downloads/App Analytics Report-06.05.2023.xlsx",sheet = 11)   
print(df9)
view(df9)
View(df9)
df9_1=df9 %>%
  top_n(20,Users)

df9 = df9[df9$`Town/City` !="(not set)",]
View(df9_1)
ggplot(df9_1, aes(x = `Town/City`)) +
  geom_bar(aes(y = `New users`, fill = "New users"), stat = "identity", position = "dodge") +
  geom_line(aes(y = `Engaged sessions`,group = 1, color = "Engaged sessions"), size = 1) +
  geom_line(aes(y = `Conversions`, group = 2 ,color = "Conversions"), size = 1) +
  scale_fill_manual(values = "blue") +
  scale_color_manual(values = c("Engaged sessions" = "green", "Conversions" = "red")) +
  labs(title = "New Users, Engaged Sessions, and Conversions by City/town", x = "City/town", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####Users by interste
df10_1=read_xlsx("C:/Users/hario/Downloads/App Analytics Report-06.05.2023.xlsx",sheet = 13)
 df10 = df10_1 %>%
   group_by(`Interests`) %>%
    top_n(15,`Interests`) %>%
   arrange(desc(`Interests`))
 
 
 #####analysis by age and gender
df11 = read_xlsx("C:/Users/hario/Downloads/App Analytics Report-06.05.2023.xlsx",sheet = 12)
 View(df11)
 
 library(ggplot2)
 

 
 # Create a pie chart
 ggplot(df11, aes(x = "", y = Users, fill = Gender)) +
   geom_bar(stat = "identity", width = 1) +
   coord_polar("y", start = 0) +  # Convert to a pie chart
   labs(title = "Gender Distribution of Users", fill = "Gender") +
   theme_minimal() +
   theme(legend.position = "right")
 
 
 
 