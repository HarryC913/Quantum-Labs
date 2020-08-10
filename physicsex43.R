ex4.data = read.csv ("ex4data.csv")
str (ex4.data)
ex4.1 = data.frame (ex4.data$Material1, ex4.data$Milivolt.Reading )
ex4.11 = data.frame (ex4.data$Material2 [c(1,2,3,4)], ex4.data$Milivolt.Reading.at.76.C [c(1,2,3,4)])
ex4.12 = data.frame (ex4.data$Material2 [c(1,2,3,4)], ex4.data$Milivolt.Reading.at.109.C [c(1,2,3,4)])
ex4.13 = data.frame (ex4.data$Material2 [c(1,2,3,4)], ex4.data$Milivolt.Reading.at.125.C [c(1,2,3,4)])
ex4.14 = data.frame (ex4.data$Material2 [c(1,2,3,4)], ex4.data$Milivolt.Reading.at.134.C [c(1,2,3,4)])
ex4.25 = data.frame (ex4.data$ [c(1)] ~ ex4.data$Milivolt.Reading.at.76.C [c(1)], ex4.data$Milivolt.Reading.at.109.C [c(1)]) ## idk whats happening here

ex4.2 = data.frame (ex4.data$Seperation..mm. [-c(11,12)], ex4.data$Light.at.10.Volts..AC...mV.reading. [-c(11,12)])
                    

ex4.3 = data.frame (ex4.data$Volts, ex4.data$Current..Amps., ex4.data$Measured.Radiation..mV.)

ex4.4 = data.frame (ex4.data$Internal.Resistance..KOhms., ex4.data$Temp..Kevs., ex4.data$Radiation..mv.)
str (ex4.2)
str (ex4.3)

par(mfrow=c(1,1))

##plot for ex 4.1
plot (ex4.1, 
      main = "Radiation of Objects of various materials and temperatures",
      xlab = "Materials found around the 200-300 physics labratory at Massey University",
      ylab = "Radiation (mV)") ##graph for radiation vs various objects

par(mfrow=c(1,4)) ## 1 x 4 graphs
## par(mfrow=c(1,1)) ## del "##" for 1 graph
plot (ex4.11, 
      ylim = c(0, 17),
      xlab = "rads @ 76 degs C")
      ## graphs for ex 4.1 radiation vs surface
plot (ex4.12, 
      ylim = c(0, 17),
      xlab = "rads @ 109 degs C")
plot (ex4.13, 
      ylim = c(0, 17),
      xlab = "rads @ 125degs C")
plot (ex4.14,  
      ylim = c(0, 17),
      xlab = "rads @ 136 degs C") ## graphs for ex 4.1 radiation vs surface

## plot for 4.1 of milivolt reading vs material

par(mfrow=c(1,1)) ## 1x1 graph
plot (ex4.data$Milivolt.Reading ~ ex4.data$Material1,
      main = "Relationship between objects and their radiation (mV)",
      xlab = "Items measured. Gathered from 200-300 level phsyics labratory Massey University", 
      ylab = " Radiaiton measurement (mV)" )


## graph for ex 4.2
plot (ex4.2,
      lines(lowess (ex4.data$Seperation..mm. [-c(11,12)], ex4.data$Light.at.10.Volts..AC...mV.reading. [-c(11,12)] , f =  1/30 ,  iter =  10, delta = 1/20*ex4.data$Seperation..mm.)) ,
      main = "Plot of inverse square law of radiation, plotting Radiation against plate and reader seperation",
      xlab = "seperation between plate and milivoltmeter (mm)", 
      ylab =  " Milivolt reading (mV)")


# plots for 4.3
plot (ex4.3$ex4.data.Volts, ex4.3$ex4.data.Measured.Radiation..mV., 
      lines ( lowess (ex4.3$ex4.data.Volts , ex4.3$ex4.data.Measured.Radiation..mV. , f =  1/30 ,  iter =  10, delta = 0.001*ex4.3$ex4.data.Volts)),
      main = "Graph Measuring the relationship between the voltage of a bulb and its emmisive radiation",
      xlab = "Light Bulb Voltage (V)", 
      ylab = "Radiation (mV)") 


     
##log of volt vs rads for 4.3
plot (log(ex4.3$ex4.data.Volts), log(ex4.3$ex4.data.Measured.Radiation..mV.), 
      xlim = c(1.6,2.5),
      lines ( lowess (log(ex4.3$ex4.data.Volts) , log(ex4.3$ex4.data.Measured.Radiation..mV.) , f = 1/3 ,  iter =  100, delta = 0.1*ex4.3$ex4.data.Volts)) ,
      main = "Graph Measuring the relationship between the voltage of a bulb and its emmisive radiation",
      xlab = "Light Bulb Voltage (V)", 
      ylab = "Radiation (mV)")


plot (ex4.3$ex4.data.Current..Amps., ex4.3$ex4.data.Measured.Radiation..mV. ,
      main =  "relationship between the current (A) through a bulb and its emmisive radiation (mV)",
      xlab = "Light Bulb Ampage (A)",
      ylab = "Radiation (mV)" )
lines(lowess (ex4.3$ex4.data.Current..Amps., ex4.3$ex4.data.Measured.Radiation..mV.  , f =  1/30 ,  iter =  10, delta = 1/20*ex4.3$ex4.data.Volts))
 

## 4,4^4? ##
plot ((ex4.4$ex4.data.Temp..Kevs.- 295.3)^4, (ex4.4$ex4.data.Radiation..mv.)^4,
      lines (lowess ((ex4.4$ex4.data.Temp..Kevs.[-c(11,12)] - 295.3)^4, (ex4.4$ex4.data.Radiation..mv.[-c(11,12)])^4 , f = 2/3, iter = 1000, delta = 0.001*(ex4.4$ex4.data.Temp..Kevs.[-c(11,12)])^4 )),
      main = "Temperature (K) vs Radiation (mV)",
      xlab = "Temperature (K) of bulb ", 
      ylab = "Radiation (mv)")


##4.4 =/= ^4 ##
plot ((ex4.4$ex4.data.Temp..Kevs.- 295.3), (ex4.4$ex4.data.Radiation..mv.),
      lines (lowess ((ex4.4$ex4.data.Temp..Kevs.[-c(11,12)] - 295.3), (ex4.4$ex4.data.Radiation..mv.[-c(11,12)]) , f = 2/3, iter = 1000, delta = 0.001*(ex4.4$ex4.data.Temp..Kevs.[-c(11,12)]) )),
      main = "Temperature (K) vs Radiation (mV)",
      xlab = "Temperature (K) of bulb ", 
      ylab = "Radiation (mv)")





