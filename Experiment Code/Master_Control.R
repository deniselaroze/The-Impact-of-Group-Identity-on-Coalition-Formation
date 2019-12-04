
###################################################################################
####################### EXPERIMENT: Gender Race and Ideology on Coaliton Formation
####################### Authors: Denise Laroze, David Hugh-Jones and Arndt Leininger
####################### Main author of this code: Denise Laroze
####################### December 2014
###################################################################################

# Created on betr version 0.2 - install_github("hughjonesd/betr", ref="v0.2")





#setwd("C:/Users/dalaro/Dropbox/Essex PhD/Paper 3/Code/Gender Race Ideology")
#setwd("C:/Users/Denise Laroze/Dropbox/Essex PhD/Paper 3/Code/Gender Race Ideology")
library(Rook) 
library(betr)
#install.packages("yaml")
library(yaml)
library(psych)
#library(reshape)
# Global values

# Treatment selection variables. 

Treatment<-"Control" #Alternatives are, Control, GRI

#Participants
N<-18 # Number of people in the session
groupsize <- 3
mg_size<-6 # Size of matching groups
m_groups<-N/mg_size  # number of matching groups
Ngroups<-mg_size/groupsize #Number of groups in each matching group

partner<-FALSE # Experimental design type

if (N %% groupsize > 0) stop("N must be an exact multiple of groupsize")
t<-10 #Number of userperiods
nreps<-5*t #total periods of the game. If the experiment it will be set to 50 (for the 10 negotiation rounds)
#for tests is just has to be a multiple of 5
userperiods<-nreps/5 ##Number of negotiation periods 

# Payoffs
pie<-17 # 
pie_d<-3
delta<-0.7 #Discount value
default<-0  #default payoff if no agreement in the 5th round of each negotiation
extrapay<-2.5  #show-up fee


#Function to show results with two decimals
specify_decimal <- function(x, k) {
  if (! is.numeric(x)) stop(traceback())
  format(round(x, k), nsmall=k) # for decimals 
}





#### Plot Ideology function

plotIdeo <- function(data) {  
  
  labels = cbind(c("You", "P2", "P3"),
                 c("P1", "You", "P3"),
                 c("P1", "P2", "You"))
  
  for (i in 1:3) {
    filename <- paste0('~/images/ideology_', data$id[i])
    png(paste0(filename, '.png'), height = 180)
    plot(0,0, xlim = c(min(data$ideology)-.5, max(data$ideology)+.5), ylim = c(0,.8), 
         type = 'n', xlab = '', ylab = '', axes = F)
    text(data$ideology, c(.5,.5,.5), labels = labels[, i])
    lines(x = c(data$ideology[1],data$ideology[1]), y = c(-.1, .3))
    lines(x = c(data$ideology[2],data$ideology[2]), y = c(-.1, .3))
    lines(x = c(data$ideology[3],data$ideology[3]), y = c(-.1, .3))
    axis(side = 1, at = c(min(data$ideology)-.5, max(data$ideology)+.5), 
         labels = c('Left', 'Right'),lwd = 2)
    dev.off()
  }
  
}


################################
# Creating expt and dataframe 
#################################


initialize_data_frame <- function() {
  mydf <<- experiment_data_frame(expt, userperiod=NA, round=NA, 
                                 instructions=NA, byear=NA, gender=NA, race=NA,  
                                 ideo1=NA,ideo2=NA,ideo3=NA,ideo4=NA,ideo5=NA,ideo6=NA,ideo7=NA,ideo8=NA,ideo9=NA,ideo10=NA,sp=NA, ideo_test=NA,
                                 noise=NA, ideology=NA, ideology.noise=NA,
                                 mg=NA, group=NA, group_id=NA, 
                                 prop1=NA, prop2=NA, prop3=NA,
                                 offer=NA, vote=NA, vote_result=NA, 
                                 pd1=NA, pd2=NA, pd3=NA, 
                                 earnings=NA, payment=NA, pay_period=NA, select_period=NA, select_earn=NA, payoffdictator=NA,
                                 final_earnings=NA_real_, demand=NA, comment=NA, session=session_name(expt), treatment=Treatment, treat_inst=NA)
 
  
  #Negotiation rounds
  mydf$userperiod<<-floor((mydf$period+3)/5)
  
  rounds<-c(rep(0, N), rep(1:5, each=N, times=userperiods), rep(0, N))
  mydf$round<<-rounds
  
  
    
  #Matching groups
  mg <- rep(1:m_groups, each=mg_size)
  mydf$mg<<-sample(mg)
  
  groups <- rep(1:Ngroups, each=groupsize)
 
  for(j in 1:m_groups){
    for (i in 0:(userperiods+1)) mydf$group[mydf$userperiod==i & mydf$mg==j] <<- sample(groups)
    }
   
 #Each groups member's id
  mydf<<-mydf[order(mydf$period,mydf$mg, mydf$group),]
  mydf$group_id <<- sequence(rle(mydf$group)$lengths) 
  mydf$group<<-as.numeric(paste0(mydf$mg, ".",mydf$group ))
  # Random noise for Ideology aspect of treatment
  mydf$noise<<-runif(nrow(mydf), min = -0.7, max = 0.7)
}

expt <- experiment(auth=TRUE, server="RookServer", port=35538, N=N, on_ready=initialize_data_frame,  clients_in_url=TRUE, name="GRIexpt", seats_file=TRUE)




# Check points. Everyone has to wait for all the participants because of the stranger matching
check_s <-checkpoint(wait_for="all")


################
### Instructions
################

inst_bf <- text_stage(page=c(
  header(), 
  "   <h2>Instructions</h2>
      <p>Welcome to the experiment.</p> 

      <p>During this experiment, please follow the instructions of the experimenters at all times. Please do not communicate 
      with any other participants or anyone outside the lab, either directly or via mobile devices. If you do not follow these 
      rules, you may be removed from the experiment without payment and you may not be allowed to participate in future experiments.</p>
               
      <p>Please switch off your mobile phones and other electronic devices.</p> 
               
      <p>Once you have finished reading the instructions, please sign the <b> consent form </b> on your desk.</p>
               
      <H3>Experiment</h3>
      <p>The experiment starts with a short survey on general demographics and your views about some political topics. 
      All the data that you enter are completely anonymous and no personal information will be recorded.</p>
      <p>Later on, you will take part in a series of group decision-making periods in which each of you will propose how to divide &pound;", specify_decimal(pie, 2),
      " amongst the members of your group. Participants will be randomly allocated to groups of ", groupsize, " by the computer. You will complete ", t, " periods 
      and in each period you will be rematched into a different group. All of the interaction with your group will be conducted via your computer. </p>
      <p>Each period will happen as follows:</p>
<br>      
<li> In the Proposal Stage you will make an offer to each participant in your group. You can offer any quantity, by increments of ten pence,
      player. The offers must add up to a 'pie' of &pound;", specify_decimal(pie, 2), "</li>
<br>
      <li>  Once all offers have been made, the computer will choose one of the proposals randomly and present it to all of the group members. If
      you accept the offer, then press the 'Accept' button. If you do not want to accept the offer, then reject it by pressing the 'Reject' button.</li>
<br>      
<li> If more than half of the group members 'Accept' the offer it will be approved and each group member will be allocated that amount for
      the current period. If more than half of the group members `Reject' the offer, it will be rejected by the group and all group members will be
      asked to propose a new division of the 'pie', but this time you will only have &pound;", specify_decimal(round(pie*delta^(2-1),1), 2),  " to divide.
      Again, one proposal will be chosen randomly and presented to all group members. If the new proposal is rejected you will repeat the process, but 
      the again 'pie' will be reduced, this time to &pound;", specify_decimal(round(pie*delta^(3-1),1), 2), ". Each of these steps is called a 'round' and you can play 
      up to 5 rounds per period, but in each round the 'pie' will get smaller. If the proposal is rejected in the fifth round, all group members will be
      allocated &pound;", specify_decimal(default, 2), " for that period and you will all pass on to a new negotiation with a new group.</li>
<br>      
<li>When you finish the 10 periods, you will be asked to complete a final decision making process. This time you will have to divide 
      &pound;", specify_decimal(pie_d, 2), " amongst three group members and, in this section, whatever you allocate to each person is what they will get. In this section there 
      will be <b>no voting</b>.</li>
<br>
      <li>At the end of the experiment we will ask you to fill in a few questions regarding your experience in the lab. Once again, all the data that you
      enter are completely anonymous and no personal information will be recorded. After you finish the survey, a screen will appear indicating
      which period was chosen for payment and how much you will be paid.</li>
<br>

<p> <b> Payment </b> </p>

 <p> In this experiment you will be paid according to the decisions that you have made. One of the ", t," negotiation 
 periods will be chosen at random, and each will have a 1/", t, " chance of being chosen. You will also be paid for the 
 decisions you make in the `decision' section and a &pound;2.50 show-up fee. At the end of the experiment you will 
 be informed of how much you have earned  in each section and your total payment. </p>  
<br>
  <form action=''><input type='Submit' value='OK'></form>", footer()
))




#########################
######## Survey Questions
#########################

ssurvey1<- function(id, period, params, errors) {
  
  if ('survey1' %in% names(params)) {
      me<-mydf$id==id
      
      mydf$gender[me] <<- if('gender' %in% names(params)) params$gender else NA
      mydf$byear[me] <<-  if('byear' %in% names(params))params$byear else NA
      mydf$uni[me] <<-  if ('uni' %in% names(params))params$uni else NA
      mydf$type_student[me] <<- if ('type_student' %in% names(params))params$type_student else NA
      mydf$startyear[me] <<- if('startyear' %in% names(params))params$startyear else NA
      mydf$degree[me] <<-  if('degree' %in% names(params))params$degree else NA
      mydf$race[me] <<- if('race' %in% names(params)) params$race else NA
      mydf$nationality[me] <<- if('nationality' %in% names(params)) params$nationality else ''
      return(NEXT)
    }    
    
  html<- header()

  html<-c(html, " <h1>Survey Questions</h1>
                  <ol>
                 <p>Please answer the following questions. As indicated in the instructions, personal data will not be disclosed and all information you provide is anonymous. </p>
                           
                 
                 <form action=''>
                 <li>What year were you born? <input type='number' name='byear' min='1930' max='2014' value=''required/></li>
                 
                 
                 <li><p> What gender do you identify with?</p></li>
                 <input type='radio' name='gender' value='male' required>Male<br>
                 <input type='radio' name='gender' value='female' required>Female<br>
                    

                 <br>
                 <li>  What is your nationality? 
                 <select name='nationality'>
                  <option value=''>-- </option>
                  <option value='Albania '>Albania </option>
                  <option value='Argentina'> Argentina </option>
                  <option value='Australia'> Australia </option>
                  <option value='Austria'> Austria</option>
                  <option value='Belgium'> Belgium</option>
                  <option value='Botswana'>Botswana </option>
                  <option value='Bulgaria'>Bulgaria</option>
                  <option value='Canada'> Canada</option>
                  <option value='Chile'>Chile</option>
                  <option value='Costa Rica'>Costa Rica </option>
                  <option value='Croatia'>Croatia </option>
                  <option value='Cyprus'>Cyprus</option>
                  <option value='Czech Republic'>Czech Republic </option> 
                  <option value='Denmark'>Denmark</option>
                  <option value='Estonia'>Estonia</option>
                  <option value='Finland'>Finland</option>
                  <option value='France'>France</option>
                  <option value='Germany'>Germany</option>
                  <option value='Greece'>Greece</option>
                  <option value='Hungary'>Hungary</option>
                  <option value='Ireland'>Ireland</option>
                  <option value='Italy'>Italy</option>
                  <option value='Jamaica'>Jamaica</option>
                  <option value='Latvia'>Latvia</option>
                  <option value='Lithuania'>Lithuania</option>
                  <option value='Luxembourg'>Luxembourg</option>
                  <option value='Macedonia'>Macedonia</option>
                  <option value='Mauritius'>Mauritius</option>
                  <option value='Montenegro'>Montenegro</option>
                  <option value='Netherlands'>Netherlands</option>
                  <option value='New Zealand'>New Zealand</option>
                  <option value='Norway'>Norway</option>
                  <option value='Poland'>Poland</option>
                  <option value='Portugal'>Portugal</option>
                  <option value='Slovak Republic'>Slovak Republic</option>
                  <option value='Slovenia'>Slovenia</option>
                  <option value='Spain'>Spain</option>
                  <option value='Sweden'>Sweden</option>
                  <option value='Switzerland'>Switzerland</option>
                  <option value='United Kingdom'>United Kingdom</option>
                  <option value='United States'>United States</option>
                  <option value='Uruguay'>Uruguay</option>
                  <option value='Other'>Other</option>
                 </select>
                  <br>


                 <li><p>What race to you identify with?<p>
                  <p>If you are mixed race, please state the one you feel closest to. 
                  </p></li>
                  <input type='radio' name='race' value='white' required>White Caucasian <br>
                  <input type='radio' name='race' value='black' required>Black <br>
                  <input type='radio' name='race' value='brown' required>Latin American <br>
                  <input type='radio' name='race' value='brown' required>South Asian <br>
                
                 <br>                
                 <li> Are you a student at the University of Essex?</li>
                 <input type='radio' name='uni' value='1'required>Yes<br>
                 <input type='radio' name='uni' value='0' required> No <br>
                 <br>
                 
                 <li>  If Yes, are you an undergraduate or graduate student?</li>
                 <input type='radio' name='type_student' value='2' required>Undergraduate<br>
                 <input type='radio' name='type_student' value='1' required>Graduate <br>
                 <input type='radio' name='type_student' value='0' required>Does not apply <br>
                 
                 <br>
                 <li>  If you are a student, in what academic year did you start your course/degree?  
                 <select name='startyears'>
                 <option value=''>-- </option>
                 <option value='Before 2007'> Before 2007</option>
                 <option value='2007/8'> 2007/8</option>
                 <option value='2008/9'> 2008/9</option>
                 <option value='2009/10'>2009/10</option>
                 <option value='2010/11'>2010/11</option>
                 <option value='2011/12'>2011/12</option>
                 <option value='2012/13'>2012/13</option>
                 <option value='2013/14'>2013/14</option>
                 <option value='2014/15'>2014/15</option>
                 </select>
                 
                 
                 <br>
                 <li>  If you are a student, what is the name of your course/degree? <textarea name='degree' rows='1' cols='60'> </textarea><br>
                 <br>
                 
                 <br></ol>
                 <input type='submit' name='survey1' value='Continue'> </form>")
 
  html<-c(html, footer())
  
  return(html)
}
  

# Ideology Questions in EXPT, used for Factor Analysis
ssurvey2<- function(id, period, params, errors) {
  
  if ('survey2' %in% names(params)) {
    me<-mydf$id==id
     
    mydf$ideo1[me] <<- if('ideo1' %in% names(params)) params$ideo1 else ''
    mydf$ideo2[me] <<- if('ideo2' %in% names(params)) params$ideo2 else ''
    mydf$ideo3[me] <<- if('ideo3' %in% names(params)) params$ideo3 else ''
    mydf$ideo4[me] <<- if('ideo4' %in% names(params)) params$ideo4 else ''
    mydf$ideo5[me] <<- if('ideo5' %in% names(params)) params$ideo5 else ''
    mydf$ideo6[me] <<- if('ideo6' %in% names(params)) params$ideo6 else ''
    mydf$ideo7[me] <<- if('ideo7' %in% names(params)) params$ideo7 else ''
    mydf$ideo8[me] <<- if('ideo8' %in% names(params)) params$ideo8 else ''
    mydf$ideo9[me] <<- if('ideo9' %in% names(params)) params$ideo9 else ''
    mydf$ideo10[me] <<- if('ideo10' %in% names(params)) params$ideo10 else ''
   
   
    mydf$ideo_test[me] <<- if('ideo_test' %in% names(params)) params$ideo_test else NA
    mydf$expts[me] <<- if('expts' %in% names(params)) params$expts else NA
    mydf$t_expts[me] <<-if('t_expts' %in% names(params)) params$t_expts else NA
          
    return(NEXT)
  }    
  
  html<- header()
  
  html<-c(html," <h1>Survey Questions Continued</h1>
		<ol>          
		<h3> &nbsp;&nbsp;&nbsp;&nbsp;Could you please state how strongly you agree or disagree with the following statements </h3>
          
          
          <form action=''>     
            
          <li><p><b> There is one law for the rich and one law for the poor.</b><span style='display:inline-block; width:290px;'></span>
          Strongly Disagree ", paste0(" <input type='radio' name='ideo1' text-align='right' float='right' value='", -2:2, "' required>", collapse=""),
          "Strongly Agree<br> </p> </li>
                    
          <li><p><b>There is no need for strong trade unions to protect employees' working conditions and wages.</b><span style='display:inline-block; width:20px;'></span>
          Strongly Disagree ", paste0(" <input type='radio' name='ideo2' value='", -2:2, "' required>", collapse=""),
          "Strongly Agree</p></li>
          
          <li><p><b>Major public services and industries ought to be in state ownership.</b><span style='display:inline-block; width: 195px;'></span>
          Strongly Disagree ", paste0(" <input type='radio' name='ideo3' value='", -2:2, "' required>", collapse=""),
          "Strongly Agree<p></li> 
          
          <li><p><b>Ordinary people get their fair share of the nation's wealth.</b><span style='display:inline-block; width: 255px;'></span>
          Strongly Disagree ", paste0(" <input type='radio' name='ideo4' value='", -2:2, "' required>", collapse=""),
          "Strongly Agree<p></li>
          
          <li><p><b>Government should reduce the taxes paid by higher-income citizens.</b><span style='display:inline-block; width: 190px;'></span>
          Strongly Disagree ", paste0(" <input type='radio' name='ideo5' value='", -2:2, "' required>", collapse=""),
          "Strongly Agree.<p></li>
          

          <li><p><b>Same sex couples should enjoy the same rights as heterosexual couples to marry.</b><span style='display:inline-block; width: 110px;'></span>
          Strongly Disagree ", paste0(" <input type='radio' name='ideo6' value='", -2:2, "' required>", collapse=""),
          "Strongly Agree<p></li>

          <li><p><b>Women should be free to decide on matters of abortion.</b><span style='display:inline-block; width: 280px;'></span>
          Strongly Disagree ", paste0(" <input type='radio' name='ideo7' value='", -2:2, "' required>", collapse=""),
          "Strongly Agree<p></li>

          <li><p><b>The government should try to reduce the income differences between rich and poor citizens.</b><span style='display:inline-block; width: 36px;'></span>
          Strongly Disagree ", paste0(" <input type='radio' name='ideo8' value='", -2:2, "' required>", collapse=""),
          "Strongly Agree<br>

          <li><p><b>The UK should be allowed to set quotas on the number of EU immigrants entering the country. </b><span style='display:inline-block; width: 10px;'></span>
          Strongly Disagree ", paste0(" <input type='radio' name='ideo9' value='", -2:2, "' required>", collapse=""),
          "Strongly Agree<br> <p></li>

          <li><p><b>Free market competition makes the health care system function better.</b><span style='display:inline-block; width: 180px;'></span>
          Strongly Disagree ", paste0(" <input type='radio' name='ideo10' value='", -2:2, "' required>", collapse=""),
          "Strongly Agree<p></li> 

	   <li><p><b>An Orange is orange.</b><span style='display:inline-block; width: 515px;'></span>
          Strongly Disagree ", paste0(" <input type='radio' name='ideo_test' value='", -2:2, "' required>", collapse=""),
          "Strongly Agree<p></li>

           </o>
          <br>
          <br> 
            

       <p> Have you ever participated in any economics, government or psychology experimental studies before? </p>
                 <input type='radio' name='expts' value='1' required>Yes<br>
                 <input type='radio' name='expts' value='0' required> No <br>
                 <br>
        <p> Please specify the number of times. If you have not participated in any experiment please indicate it with a zero. </p>
                 <input type='number' name='t_expts' min='0' value=''required /> 
                 <br>
                 <br>
               
           <input type='submit' name='survey2' value='Continue'> </form>")
 
  html<-c(html, footer())
  
  return(html)
  
  
  }
  

ssurvey3<- function(id, period, params, errors) {
  
  if ('survey3' %in% names(params)) {
    me<-mydf$id==id
    mydf$sp[me] <<- if('sp_point' %in% names(params)) as.numeric(params$sp_point) else ''
    return(NEXT)
  }    
  
  html<- header()
  
  html<-c(html, " 

        <h1>Survey Questions Continued</h1>
        <p> In politics people sometimes talk of 'left' and 'right'. Where would you place yourself on a scale from 0 to 10 where 0 means extreme left and 10 means extreme right? </p>
      <form method='post' action=''>

        <p>Extreme Left 0 <input type='range' id='rangeInput' name='sp_point' value='5' min='0' max='10' value='0' oninput='amount.value=sp_point.value' required> 10  Extreme Right</p>
        <br>                                                     
        <p>You have selected: <span style='color:red'><output name='amount' for='sp_point'></output></span></p>
        <br> 
        <br>
        <br> 
        <br> 
        <input type='submit' name='survey3'  value='Continue' >
        </form>
        ")
  
  html<-c(html, footer())
  
  return(html)
  
  
}

##################################
########  Calculation of Ideology 
##################################

ideology <- program("last", function(id, period) {

  mydf$ideology <<- as.numeric(mydf$sp) ##inclusion of ideology position calculation
  me<-mydf$id==id
  mydf$ideology.noise<<-mydf$ideology+mydf$noise  ### Noise is added to add randomness to the position of participants and avoid problems with re-maching in small groups
})


### Information about the Avatars to the "Treatment" Groups
Treat_info<- function(id, period, params, errors) {
  me_now<-mydf$id==id & mydf$period==period
  
  if ('treat_inst' %in% names(params)) {
    mydf$treat_inst[me_now]<<- "Read"
    return(NEXT)
  } 
  
  
  html<- header()
  
  html<-c(html, " <h1>Important Information</h1>
          <p>The information you have provided has been used to allocate each participant
          one of the following avatars.</p>
           <a href='https://deniselaroze.files.wordpress.com/2014/03/avatar-set-1.png'><img src='https://deniselaroze.files.wordpress.com/2014/03/avatar-set-1.png?w=300' alt='Avatar set 1' width='300' height='223' class='alignnone size-medium wp-image-57' /></a>
 
          <p>There are only 6 avatars, so more than one participant will be allocated
          the same image, based on the race and gender each person stated in the
          survey. </p>
          <p>The survey data has also been used to calculate a score that places each 
          participant and their political views on the left right political spectrum. 
          </p>
          <form><input type='submit' name='treat_inst' value='Continue'> </form>")
  
  
  
          
  html<-c(html, footer())
  
  if(Treatment=="GRI" && is.na(mydf$treat_inst[me_now])) return(html) else return(NEXT)
  
  
}




######################################################
##########################Baron and Farejohn 1989 Game
######################################################



### Proposal Stage
fun_prop <- function(id, period, params, errors) {
  me_now <- mydf$id==id & mydf$period==period 
  me_pm1 <- mydf$id==id & mydf$period==period-1
  
  
  mygroup <- mydf$group[mydf$id==id & mydf$period==period]
  mygroup_id<- mydf$group_id[mydf$id==id & mydf$period==period & mydf$group==mygroup]
  me_prop<-mydf$group==mygroup & mydf$period==period
  proposals3 <- paste0("prop_", 1:3)
  offer<-sample(proposals3[1:groupsize],1) 
  mydf$offer[me_prop]<<-offer
 
  
  if ('proposal' %in% names(params)) {
    propnames <- paste0("prop", 1:groupsize) 
    prop <<- unlist(params[propnames])
    mydf[me_now, propnames] <<- as.numeric(prop)
    } 
    
  
 html<-c(header(),
          sprintf("<h2>This is negotiation period %s and round %s </h2>", mydf$userperiod[me_now], mydf$round[me_now]),
          sprintf("<p>Split <b> %s pounds </b> among the participants in your group.
                  Type in the amount you would like to offer each participant, including yourself, 
                  in the boxes below.</p>",  specify_decimal(round(pie*delta^(mydf$round[me_now]-1),1), 2)), 
          sprintf("<p><b>You are participant %s</b> </p>",mydf$group_id[me_now]),  
          "<br>")


#Treatments, images of Avatars and idelogical position
  if(Treatment=="GRI") {
    ps <- mydf[mydf$group==mygroup & mydf$period==period,] # 3 rows
    plotIdeo(ps) ## this function generates the ideology images
   
    avatar_files <- paste(ps$race, "_", ps$gender, "_1.png", sep="")
    rows_i_want <- setdiff(1:3, mygroup_id)
        
    front<-"<div id='images' style='width: 130px; text-align: right; float:right;'>"
    back1<- paste0(" <div class='caption'> Participant ", rows_i_want[1] ," </div></a>", sep="")
    back2<- paste0(" <div class='caption'> Participant ", rows_i_want[2] ," </div></a>", sep="")
    
    
    other1<-paste0(front, "<a href='https://deniselaroze.files.wordpress.com/2014/03/" , avatar_files[rows_i_want[1]], "'><img src='https://deniselaroze.files.wordpress.com/2014/03/" , avatar_files[rows_i_want[1]], "' alt='" , avatar_files[rows_i_want[1]], "' width='91' height='121' class='alignnone size-full' align='right' /> </a", back1)
    other2<-paste0(front, "<a href='https://deniselaroze.files.wordpress.com/2014/03/" , avatar_files[rows_i_want[2]], "'><img src='https://deniselaroze.files.wordpress.com/2014/03/" , avatar_files[rows_i_want[2]], "' alt='" , avatar_files[rows_i_want[2]], "' width='91' height='121' class='alignnone size-full' align='right' /> </a", back2)
    
    plotname<-paste0("/images/ideology_",mydf$id[me_now], ".png", sep="")
    idplot<-paste0("<div id='images' style='text-align: right; float:right;'><img src='", plotname,"' alt='Ideology Plot'/> </div>")
    html<-c(html,idplot, other1, other2)
       
  }
    
  

 html<-c(html,  "<form action='' method='post'> 
          <p>", ifelse(mydf$group_id[me_now]==1, "Offer to yourself: &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp ", "Offer to Participant 1: "),
          "<input type='number' autocomplete='off' step='0.1' min='0' max='", round(pie*delta^(mydf$round[me_now]-1), 1) ,"' name='prop1' value='' required></p>
          <p>", ifelse(mydf$group_id[me_now]==2, "Offer to yourself: &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp ", "Offer to Participant 2: "),
          "<input type='number' autocomplete='off' step='0.1' min='0' max='", round(pie*delta^(mydf$round[me_now]-1), 1) ,"' name='prop2' value='' required></p>
          <p>", ifelse(mydf$group_id[me_now]==3, "Offer to yourself: &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp ", "Offer to Participant 3: "),
          "<input type='number' autocomplete='off' step='0.1' min='0' max='", round(pie*delta^(mydf$round[me_now]-1), 1),"'name='prop3' value='' required></p>"
          )
  
  html<-c(html,"<p style='color:red'>", paste(errors, collapse="<br />"), "</p><input type='Submit' name='proposal'  value='Submit'></form>",footer())                               
  
   
  
  prop1 <- if ('prop1' %in% names(params)) as.numeric(params$prop1) else ''
  prop2 <-  if ('prop2' %in% names(params)) as.numeric(params$prop2) else ''
  prop3 <-  if ('prop3' %in% names(params)) as.numeric(params$prop3) else ''
 
  ### Sequence of periods. Participants move on to the next negotiation period (userperiod) if the group accepts the 
  # proposal, but continue on the negotiation if it is rejected. 
  if(mydf$round[me_now]==1)  return(html) 
  else if (mydf$vote_result[me_pm1]=="Accepted" || mydf$vote_result[me_pm1]=="Not Applicable") return(NEXT)
  else  return(html) 
  
                              
} 

### Checking for Errors
prop_s <- form_stage(page=fun_prop,
              fields=list(
                prop1=all_of(has_value(), is_between(0, 17)),
                prop2=all_of(has_value(), is_between(0, 17)),
                prop3=all_of(has_value(), is_between(0, 17) , function(field_title, value, id, period, params) {
                  if ('prop3' %in% names(params)) {
                    props.all<-as.numeric(c(params$prop1, params$prop2, params$prop3))
                    r<-mydf$round[mydf$id==id & mydf$period==period]
                    mysum<- sum(props.all)
                    pie_round<-pie*delta^(r-1)
                    pie_round<-round(pie_round, 1)
                    if (mysum == pie_round) return(NULL) else return(sprintf("Your offer does not add up to %s! The numbers you entered add up to %s", pie_round, mysum))
                  }
                  else return("Please enter a value into prop3!")
                }
                )
                ), data_frame="mydf")
                



# Selected offer/vote stage

offer_s<- function(id, period, params, errors) {
  me_now <- mydf$id==id & mydf$period==period
  me_pm1 <- mydf$id==id & mydf$period==period-1
  Accept<-"Accept"
  Reject<-"Reject"
  ###
  
  mygroup <- mydf$group[mydf$id==id & mydf$period==period]
  mygroup_id<- mydf$group_id[mydf$id==id & mydf$period==period & mydf$group==mygroup]
    
  offer<- mydf$offer[me_now]       
 
  
  prop_id <- as.numeric(sub("prop_", "", mydf$offer[me_now]))
  
  prop_p <- numeric(0)
  for (i in 1:groupsize) prop_p[i] <- which(mydf$group_id==i & mydf$period==period & mydf$group==mygroup)
  
  ## This generates a matrix for each group
  roni <- as.matrix(t(mydf[prop_p, paste0("prop", 1:groupsize)]))
    value_ply <- as.numeric(roni[, prop_id])
  
  
  if ('vote' %in% names(params)) {
    mydf$vote[me_now]<<- params$vote
    return(NEXT)
    }
  
    
 html<-c( 
  header(), sprintf("<h2>This is negotiation period %s and round %s </h2>", mydf$userperiod[me_now], mydf$round[me_now]))
  
  
  if(Treatment=="GRI") {
    ps <- mydf[mydf$group==mygroup & mydf$period==period,] # 3 rows
    avatar_files <- paste(ps$race, "_", ps$gender, "_1.png", sep="")
    rows_i_want <- setdiff(1:3, mygroup_id)
    
    front<-"<div id='images' style='width: 130px; text-align: right; float:right;'>"
    back1<- paste0(" <div class='caption'> Participant ", rows_i_want[1] ," </div></a>", sep="")
    back2<- paste0(" <div class='caption'> Participant ", rows_i_want[2] ," </div></a>", sep="")
    
    
    other1<-paste0(front, "<a href='https://deniselaroze.files.wordpress.com/2014/03/" , avatar_files[rows_i_want[1]], "'><img src='https://deniselaroze.files.wordpress.com/2014/03/" , avatar_files[rows_i_want[1]], "' alt='" , avatar_files[rows_i_want[1]], "' width='91' height='121' class='alignnone size-full' align='right' /> </a", back1)
    other2<-paste0(front, "<a href='https://deniselaroze.files.wordpress.com/2014/03/" , avatar_files[rows_i_want[2]], "'><img src='https://deniselaroze.files.wordpress.com/2014/03/" , avatar_files[rows_i_want[2]], "' alt='" , avatar_files[rows_i_want[2]], "' width='91' height='121' class='alignnone size-full' align='right' /> </a", back2)
    
    plotname<-paste0("/images/ideology_",mydf$id[me_now], ".png", sep="")
    idplot<-paste0("<div id='images' style='text-align: right; float:right;'><img src='", plotname,"' alt='Ideology Plot'/> </div>")
    html<-c(html,idplot, other1, other2)
    
    
  }
  
  
  html<-c(html,
          "<p>The following offer has been selected at random from the proposals made in the previous section.</p>",
          if(mydf$group_id[me_now]==prop_id) "<b>Your offer is </b><br> " else sprintf(" <b> Participant %s's offer is:</b><br>", prop_id),  
          sprintf("<p> %s : %s", ifelse(mydf$group_id[me_now]==1, "You get &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ", "Participant 1 gets") , specify_decimal(value_ply[1], 2)),
          sprintf("<p> %s : %s", ifelse(mydf$group_id[me_now]==2, "You get &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ", "Participant 2 gets") , specify_decimal(value_ply[2], 2)),
          sprintf("<p> %s : %s", ifelse(mydf$group_id[me_now]==3, "You get &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ", "Participant 3 gets"), specify_decimal(value_ply[3], 2)),
          "<p> Click the appropriate button to accept or reject the offer.</p>
               <form enctype='multipart/form-data' action='' method='POST'>
               Choose an action:<br>
               <button type='submit' name='vote' value='1'>", Accept, "</button> 
               <button type='submit' name='vote' value='0'>", Reject, "</button>
              </form>", footer()
    )
  
  ### Sequence of periods. Participants move on to the next negotiation period (userperiod) if the group accepts the 
  # proposal, but continue on the negotiation if it is rejected. 
  if(mydf$round[me_now]==1)  return(html) 
  else if (mydf$vote_result[me_pm1]=="Accepted" || mydf$vote_result[me_pm1]=="Not Applicable") return(NEXT)
  else  return(html) 
     


  
}




### Result Stage
result_s <- function(id, period, params, errors) {
  
  me_now <- mydf$id==id & mydf$period==period 
  me_pm1 <- mydf$id==id & mydf$period==period-1
  mygroup <- mydf$group[mydf$id==id & mydf$period==period]
  mygroup_id <- mydf$group_id[mydf$id==id & mydf$period==period]
  
  vote_s4<-mydf$period==period & mydf$group==mygroup 
 
 
  vote<- as.vector(mydf$vote[vote_s4])
  vote<-as.numeric(vote)
 
  vote_res_pid<-function(vote) {
     if(sum(vote) < groupsize/2) return(default)
      x <- mydf$offer[me_now]
      x <- sub("prop_", "", x)
      x <- as.integer(x)
      return(as.numeric(roni[mygroup_id, x]))
    }
    
  
  
  if(is.na(mydf$vote[me_now])){
    mydf$vote_result[me_now] <<- "Not Applicable"
    } else {
      if(sum(vote) >= groupsize/2) vote_result <-"Accepted" else vote_result <-"Rejected"
      ply_s4 <- which(mydf$group_id %in% 1:groupsize & mydf$period==period & mydf$group==mygroup)
      #Result for voting
      ## Repeat proposal matrix for each group
      roni <- as.matrix(t(mydf[ply_s4, paste0("prop", 1:groupsize)]))
      # This function calculates the payoffs for each participant
      mydf$vote_result[me_now] <<- as.character(vote_result)
      mydf$earnings[me_now] <<- as.numeric(vote_res_pid(vote)) 
    } 
  
  
  if ('moveon' %in% names(params)) {
       mydf$moveon[me_now]<<- params$moveon
        return(NEXT)} 
    
  html<-header()
  
  if (mydf$vote_result[me_now]=="Accepted"){
    html<-c(html, 
            sprintf("<h2>This is negotiation period %s and round %s </h2>", mydf$userperiod[me_now], mydf$round[me_now]),
            sprintf("</b><p>The proposal has been <b> %s </b>  <p>", mydf$vote_result[me_now]),  
            sprintf("</b></p><p>You have obtained a chance of winning %s </b>", specify_decimal(mydf$earnings[me_now], 2)),
            " pounds from this round. Now you will move onto a different negotiation period and you will be remached with different people </p>
            <br> Final results will be calculated at the end of the session</p>
              <p>Press the 'Next Negotiation' button to go on to the next step.</p>
          <form> <input type='submit' name='moveon' value='Next Negotiation'> </button></form>",footer())
    mydf$payment[me_now]<<-1
  }                    
  else if (mydf$vote_result[me_now]=="Rejected" && mydf$round[me_now] <5 ) { html<-c(header(), 
                 sprintf("<h2>This is negotiation period %s and round %s </h2>", mydf$userperiod[me_now], mydf$round[me_now]),
                 sprintf("</b><p>The proposal has been <b> %s </b>  <p>", mydf$vote_result[me_now]),  
                 sprintf("<p> You will now have a new chance of reaching an agreement, but the amount of money you 
                  can split has been reduced to %s </p>
                  <p>Press the 'Next Negotiation' button to go on to the next step.</p>", specify_decimal(round(pie*delta^(mydf$round[me_now]), 1), 2)),
                 "<form> <input type='submit' name='moveon' value='Next Negotiation'> </button></form>",footer()) 
  }    
 
  
  else { html<-c(header(), 
                 sprintf("<h2>This is negotiation period %s and round %s </h2>", mydf$userperiod[me_now], mydf$round[me_now]),
                 sprintf("</b><p>The proposal has been <b> %s </b>  <p>", mydf$vote_result[me_now]),  
                 sprintf("<p>You have obtained a chance of winning %s pounds from this negotiation period. Now you will move onto
                 a the next negotiation period and you will be remached with different people </p>
                 <p>Final results will be calculated at the end of the session</p>
                  <p>Press the 'Next Negotiation' button to go on to the next step.</p>", specify_decimal(default, 2)),
                  "<form> <input type='submit' name='moveon' value='Next Negotiation'> </button></form>",footer()) 
         if(mydf$vote_result[me_now]=="Rejected") mydf$payment[me_now]<<-1 else mydf$payment[me_now]<<-NA
  }  
  
  
  
  ### Sequence of periods. Participants move on to the next negotiation period (userperiod) if the group accepts the 
  # proposal, but continue on the negotiation if it is rejected. 
  if(mydf$round[me_now]==1)  return(html) 
  else if (mydf$vote_result[me_pm1]=="Accepted" || mydf$vote_result[me_pm1]=="Not Applicable") return(NEXT)
  else  return(html)  
   
  } 




######################################
###################### Dictator Game
######################################

#Instructions
inst_d <- text_stage(page=c(
  header(), 
  " <h2>Instructions Third Stage</h2>","<p>In this section you will have to divide &pound;", specify_decimal(pie_d, 2), " amongst three people, but there will be <b>no voting</b>. 
  Whatever you allocate to each person is what they will recieve, including yourself. </p> 
    <p> Please press the continue button to move on</p> 
  
  <form action=''><input type='Submit' value='OK'></form>",
  footer()
))

#### Division of Pie
fun_decision_d <- function(id, period, params, errors) {
  
  me_now <- mydf$id==id & mydf$period==period
  mygroup <- mydf$group[mydf$id==id & mydf$period==period]
  me_prop<-mydf$group==mygroup & mydf$period==period
  me<- mydf$id==id
      
  if ('decision' %in% names(params)) {
    propnamesd <- paste0("pd", 1:groupsize) 
    pd <- unlist(params[propnamesd]) 
    mydf[me_now, propnamesd] <<- as.numeric(pd)
    
  }
    pd1 <- if ('pd1' %in% names(params)) as.numeric(params$pd1) else ''
    pd2 <-  if ('pd2' %in% names(params)) as.numeric(params$pd2) else ''
    pd3 <-  if ('pd3' %in% names(params)) as.numeric(params$pd3) else ''
  
  
  
   
  
  html<-header()
   
  html<-c(html, 
          "<h2>This is a decision period 1 </h2>",
          
          sprintf("<p>Split <b> %s pounds </b> among the participants in your group.
                          Type in the amount you would like to offer each participant, including yourself, 
                          in the boxes below.</p>",  specify_decimal(pie_d, 2)), 
          sprintf("<p><b>You are participant %s</b> </p>",mydf$group_id[me_now]), 
          "<form action='' method='post'> 
                  <p>", ifelse(mydf$group_id[me_now]==1, "Keep for yourself: &nbsp;&nbsp;&nbsp;&nbsp ", "Give to Participant 1: "),
          "<input type='number' autocomplete='off' step='0.1' min='0' max='3' name='pd1' value='' required></p></p>
                  <p>", ifelse(mydf$group_id[me_now]==2, "Keep for yourself: &nbsp;&nbsp;&nbsp;&nbsp ", "Give to Participant 2: "),
          "<input type='number' autocomplete='off' step='0.1' min='0' max='3' name='pd2' value='' required></p></p></p>
                  <p>", ifelse(mydf$group_id[me_now]==3, "Keep for yourself: &nbsp;&nbsp;&nbsp;&nbsp ", "Give to Participant 3: "),
          "<input type='number' autocomplete='off' step='0.1' min='0' max='3' name='pd3' value='' required></p></p>")

    
  html<-c(html,"<p style='color:red'>", paste(errors, collapse="<br />"), "<br><input type='submit' name='decision'  value='Submit'></form>",footer())                               
  
  return(html)                             
} 

### Checking for Errors
prop_sd <- form_stage(page=fun_decision_d, fields=list(
  pd1=all_of(has_value(), is_between(0, 3)),
  pd2=all_of(has_value(), is_between(0, 3)),
  pd3=all_of(has_value(), is_between(0, 3), function(field_title, value, id, period, params) {
    if ('pd3' %in% names(params)) {
      propnamesd <- paste0("pd", 1:groupsize) 
      pd <- unlist(params[propnamesd]) 
      pd.all<-as.numeric(c(params$pd1, params$pd2, params$pd3))
      mysum_d<- sum(pd.all)
      if (mysum_d == pie_d) return(NULL) else return(sprintf("Your offer does not add up to %s! The numbers you entered add up to %s", 3, mysum_d)) 
    }
    else return("Please enter a value into pd3!")
  }) 
 
), data_frame="mydf")


#### Some final questions
ssurvey4<- function(id, period, params, errors) {
  
  if ('survey4' %in% names(params)) {
    me_p1<-mydf$id==id & mydf$period==1
    mydf$demand[me_p1]<<-if("demand" %in% names(params)) as.character(params$demand) else 'NA'
    mydf$comment[me_p1]<<-if("comment" %in% names(params)) as.character(params$comment) else 'NA'
    return(NEXT)
  }    
  html<-header()
  
  html<-c(html, "
          <h2> Please take a few minutes and to answer the following questions </h2>
          
          <form action='' method='post'>  
          <p>  From your experience, what did you think the experiment was about? </p>
          <textarea name='demand' rows='9' cols='60'> </textarea>
          
          <p> What was your overall impression of the experiment? </p>
          <textarea name='comment' rows='9' cols='90'> </textarea> 
          
          <br>
          <input type='submit' name='survey4' value='Submit'> </form>",
          footer()
  )
  return(html)
  
  
}










#############################
############# Final Earnings  
#############################

payment_calc <- program("all", function(id, period) {
  me_now <- mydf$id==id & mydf$period==period  
  me_p2<- mydf$id==id & mydf$period==2
  me<- mydf$id==id
  
  me_now <- mydf$id==id & mydf$period==period  
  mygroup <- mydf$group[mydf$id==id & mydf$period==period]
  mygroup_id<- mydf$group_id[mydf$id==id & mydf$period==period & mydf$group==mygroup]
  me_p2<- mydf$id==id & mydf$period==2
  me<- mydf$id==id
  
  negotiation_periods<-c(2:(nreps+1))
  
  ### From BF Game
  mydf$pay_period<<- ifelse(is.na(mydf$payment), NA, mydf$period)
  paypd<-na.omit(mydf$pay_period[me])
  pp <- if(nreps<=5) paypd else sample(paypd, 1)  
  mydf$select_period[me] <<- pp
  select_period<-mydf$select_period[me]
  mydf$select_earn[me] <<- mydf$earnings[mydf$id==id & mydf$period==select_period]
  
  
  #Payoff Dictator Game
  payoffdictator<-sum(as.numeric(mydf[[paste0("pd", mygroup_id, "")]][mydf$group==mygroup & mydf$period==period]))
  mydf$payoffdictator[me]<<-payoffdictator
  
  #Sum Earnings
  mydf$final_earnings<<- as.numeric(ifelse (! is.na(mydf$select_earn), mydf$select_earn+extrapay+mydf$payoffdictator, NA))
  
  
  
})


print <- program("last", function(id, period) {
  
  #Writing csv of the results
  mydf <<- merge_subjects(expt, mydf)
  mydf<<-mydf[order(mydf$period,mydf$mg, mydf$group),]
  file.name <- paste0(session_name(expt), ".csv")
  write.csv(mydf,file=file.name,row.names=F) 
  
  profits <- mydf[mydf$period==1, c("final_earnings", "seat", "client")]
  profits <- profits[order(profits$seat),]
  write.csv(profits, file="profits.csv")
  
})

##### Final Payment Period
end_s<- function(id, period, params, errors) {
  me_now <- mydf$id==id & mydf$period==period  
  me_p2<- mydf$id==id & mydf$period==2
  
  if ('end' %in% names(params)) return(NEXT)

  
  html<-header()
  
  html<-c(html, "<h1>Final Payoff</h1>",  
                sprintf(" <p>As indicated in the instructions, one negotiation period has randomly been chosen for payment. 
                In your case negotiation <b> %s </b> was selected, which means you will obtain <b> %s pounds</b> from the negotiation section.</p>
                <p> You will also be paid <b> %s pounds </b> for the decision section and <b> %s pounds </b> for a show-up fee. 
                In total you will leave with <b> %s pounds </b> for your participation in this
                experiment.</p>", mydf$userperiod[mydf$id==id & mydf$period==mydf$select_period[me_now]], specify_decimal(mydf$select_earn[me_p2], 2), mydf$payoffdictator[me_now],
                extrapay,specify_decimal(mydf$final_earnings[me_now], 2)))
   
          
               
  html<-c(html,"<form> <input type='submit' name='end' value='End'> </button></form>",footer())                               
  
  
}


##### Calculations and printing




add_stage(expt, period(),inst_bf, check_s, ssurvey1, ssurvey2, ssurvey3, ideology, Treat_info, times=1)
add_stage(expt, period(),check_s, prop_s, check_s, offer_s, check_s, result_s, check_s, times=nreps)
add_stage(expt, period(),inst_d, prop_sd, ssurvey4, check_s, payment_calc, check_s, print, end_s, times=1)


