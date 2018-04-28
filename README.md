# ipl_record
data analysis

matches <- read_csv("~/Desktop/r_sudios/New folder/hw/matches.csv")
View(matches)

#1. Print total no of matches for the 2008 season
count=0
for(i in matches$season){ if(i==2008) count=count+1}
cat("Total no. of matches is: ",count)

#2. Print the city name where maximum /minimum no of matches conducted.
a=as.data.frame(table(matches$city))
cat("the highest no. of matches were conducted in")
print(subset(a,a$Freq==max(a$Freq)))

#3. Print total count of citywise. (Ex: Jaipur 10)
print(as.data.frame(table(matches$city)))

#4. Find the team name from the columns team1 and team2 where any 1 alphabet will repeat 2 times.(Ex: Deccan Chargers , Chennai Super Kings)
#######................................................dt
#5. Find which team  is maximum and minimum  toss winner.
e=as.data.frame(table(matches$toss_winner))
cat("the highest no. of tosses were won by \n")
print(subset(e,e$Freq==max(e$Freq)))
cat("the lowest no. of tosses were won by ")
print(subset(e,e$Freq==min(e$Freq)))

#6. Find maximum/minimum toss winner team on city basis.      .......................dt
h=as.data.frame(table(matches$city))
j=subset(h,h$Freq==max(h$Freq))
k=subset(h,h$Freq==min(h$Freq))
cat("the highest no. of toss winner on city basis is/are")
print(subset(matches,i$Var1==matches$city,select = c(7,3)))
#cat("the lowest no. of toss winner on city basis is/are ")
#print(subset(matches,jVar1==matches$city,select = c(7,3)))

#7. Check if any team is toss winner then that team will go for  bat or field i.e. print the table for toss winning team name along with bat option.
l=subset(matches,select = c(7,8))
View(l)

#8. Print the total count of normal/tie matches from the column result and print the team names where the match result is tie.
tie_matches=0
normal_matches=0
for(i in matches$result){ if(i=="tie") tie_matches=tie_matches+1
else if(i=="normal") normal_matches=normal_matches+1}
cat("Total no. of tie matches is: ",tie_matches)
cat("Total no. of normal matches is: ",normal_matches)

#9. Print total count of 0/1 from the column dl_applied
zero_count=0
one_count=0
for(i in matches$dl_applied){ if(i==0) zero_count=zero_count+1
else if(i==1) one_count=one_count+1}
cat("total count of 0 from the column dl_applied is:",zero_count)
cat("total count of 1 from the column dl_applied is:",one_count)

#10. Print the team name who won the match by highest run and lowest run.
m=subset(matches,matches$win_by_runs==max(matches$win_by_runs),select = c(11))
n=subset(matches,matches$win_by_runs==min(matches$win_by_runs),select = c(11))
cat("team winning by highest runs i.e. ",max(matches$win_by_runs),"runs is/are")
print(m)
cat("team winning by lowest runs i.e. ",min(matches$win_by_runs),"runs is/are")
print(n)

#11. Print the team names who won the match with the double digit and both are the same.(Ex: 66,33)
o=subset(matches,(matches$win_by_runs%%11==0)&(matches$win_by_runs>10)&(matches$win_by_runs<100))
View(o)

#12. Print the team name who won the match by highest wicket and lowest wicket.
p=subset(matches,matches$win_by_wickets==max(matches$win_by_wickets),select = c(11))
q=subset(matches,matches$win_by_wickets==min(matches$win_by_wickets),select = c(11))
cat("team winning by highest wickets i.e. ",max(matches$win_by_wickets),"wickets is/are")
print(p)
cat("team winning by lowest wickets i.e. ",min(matches$win_by_wickets),"wickets is/are")
print(q)

#13. Print the team name who won the match by 0 wicket.
r=subset(matches,matches$win_by_wickets==0,select = c(11))
cat("team winning by 0 wickets is/are")
print(r)

#14. Print the name of the player who was player of match for more than 3 times.
s=as.data.frame(table(matches$player_of_match))
cat("the name of player with no. of player of match>3 is/were won by \n")
print(subset(s,s$Freq>3))

#15. Print the name of the player who was player of match for maximum and minimum times.
t=as.data.frame(table(matches$player_of_match))
cat("the highest no. of player of match is/were won by \n")
print(subset(t,t$Freq==max(t$Freq)))
cat("the lowest no. of player of match is/were won by ")
print(subset(t,t$Freq==min(t$Freq)))

#16. Print the venue names where  the team will won the match  by highest run and highest wicket.
print(subset(matches,(matches$win_by_runs==max(matches$win_by_runs))&(matches$win_by_wickets==max(matches$win_by_wickets)),select = c(3,11,12,13)))

#17. Print the venue names where  the team will won the match  by lowest run and highest wicket.
print(subset(matches,(matches$win_by_runs==min(matches$win_by_runs))&(matches$win_by_wickets==max(matches$win_by_wickets)),select = c(3,11,12,13)))

#18. Print umpire names who did the umpiring for maximum and minimum times.

#as combination of umpire 1 and 2
u=as.data.frame(table(matches$umpire1,matches$umpire2))
cat("the highest no. of combination of umpire 1 and 2 is/were done by ")
print(subset(u,u$Freq==max(u$Freq)))
cat("the lowest no. of combination of umpire 1 and 2 is/were done by")
print(subset(u,u$Freq==min(u$Freq)))

#as only umpire 1
v=as.data.frame(table(matches$umpire1))
cat("the highest no. of umpire 1 is/were done by ")
print(subset(v,v$Freq==max(v$Freq)))
cat("the lowest no. of umpire 1 is/were done by")
print(subset(v,v$Freq==min(v$Freq)))

#as only umpire 2
w=as.data.frame(table(matches$umpire2))
cat("the highest no. of umpire 2 is/were done by ")
print(subset(w,w$Freq==max(w$Freq)))
cat("the lowest no. of umpire 2 is/were done by")
print(subset(w,w$Freq==min(w$Freq)))


