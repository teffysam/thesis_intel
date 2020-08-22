# script for visualizing diagnostic output
# for gnuplot 3.5 (linux version)
# j. behrens 7/94, 5/97
#
# no. of elements
set autoscale;
set title "Flash90 total No. of Elements";
set xlabel "timestep no.";
set ylabel "no. of elements";
plot 'Flash90_diag.0000' us 1:2 wi lines;
pause -1 "ENTER to continue";
#
# no. of elements
set title "Flash90 No. of Elements (fine grid)";
set xlabel "timestep no.";
set ylabel "no. of elmnts";
plot 'Flash90_diag.0000' us 1:3 wi lines;
pause -1 "ENTER to continue";
#
# no. of edges
set title "Flash90 total No. of Edges";
set xlabel "timestep no.";
set ylabel "no. of edges";
plot 'Flash90_diag.0000' us 1:4 wi lines;
pause -1 "ENTER to continue";
#
# no. of edges
set title "Flash90 No. of Edges (fine grid)";
set xlabel "timestep no.";
set ylabel "no. of edges";
plot 'Flash90_diag.0000' us 1:5 wi lines;
pause -1 "ENTER to continue";
#
# no. of nodes
set title "Flash90 No. of Nodes";
set xlabel "timestep no.";
set ylabel "no. of nodes";
plot 'Flash90_diag.0000' us 1:6 wi lines;
pause -1 "ENTER to continue";
#
# Min
set title "Flash90 Minimum of u";
set xlabel "timestep no.";
set ylabel "Min.";
plot 'Flash90_diag.0000' us 1:7 wi lines;
pause -1 "ENTER to continue";
#
# Max
set title "Flash90 Maximum of u";
set xlabel "timestep no.";
set ylabel "Max.";
plot 'Flash90_diag.0000' us 1:8 wi lines;
pause -1 "ENTER to continue";
#
# rfm
set title "Flash90 RFM";
set xlabel "timestep no.";
set ylabel "rfm";
#set noautoscale y;
#set yrange [0.95:1.05];
plot 'Flash90_diag.0000' us 1:9 wi lines;
pause -1 "ENTER to continue";
#
# rsm
set title "Flash90 RSM";
set xlabel "timestep no.";
set ylabel "rsm";
#set autoscale;
plot 'Flash90_diag.0000' us 1:10 wi lines;
pause -1 "ENTER to continue";
#
# Max Norm Error
set title "Flash90 Error (Max-Norm)";
set xlabel "timestep no.";
set ylabel "maximum norm";
plot 'Flash90_diag.0000' us 1:11 wi lines;
pause -1 "ENTER to continue";
#
# L2 Norm Error
set title "Flash90 Error (L2-Norm)";
set xlabel "timestep no.";
set ylabel "L2 norm";
plot 'Flash90_diag.0000' us 1:12 wi lines;
pause -1 "ENTER to continue";
#
# Diffusion Error
set title "Flash90 Diffusion Error";
set xlabel "timestep no.";
set ylabel "diff. error";
plot 'Flash90_diag.0000' us 1:13 wi lines;
pause -1 "ENTER to continue";
#
# Dispersion Error
set title "Flash90 Dispersion Error";
set xlabel "timestep no.";
set ylabel "disp. error";
plot 'Flash90_diag.0000' us 1:14 wi lines;
pause -1 "ENTER to continue";
