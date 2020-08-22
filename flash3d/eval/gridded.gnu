##################################################################
# SPLASH/fe                                                      #
# Semi-Lagrangian Parallel and Locally Adaptive                  #
# SHallow-water model/ with Finite Elements                      #
##################################################################
# script for visualizing stasl output                            #
# for gnuplot 3.5 (linux version)                                #
# j. behrens 7/94, 3/96                                          #
##################################################################
#set noparametric;
set hidden3d;
set view 30,322.5,1,2;
set title "FLASH Data Plot";
splot 'Flash90_matlab.0001' with lines;
pause -1 "ENTER to continue";
#----------------------------------------------------------------#
# END                                                            #
#----------------------------------------------------------------#
