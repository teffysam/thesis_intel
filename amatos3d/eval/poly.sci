// An M-File to postprocess data written by STASL
// Visualization in 3D
// j. behrens 10/96
//
// clear old data structures
clear;
//
// open file and read data
disp('reading data ...');
RAW = read('infile.dat',-1,7);
disp('... done, now processing grid ...');
//
// determine sizes
[m,n]= size(RAW);
//
// loop through elements
for i=1:m,
  for j=1:3,
    XX(j,i)= RAW(i,j);
    YY(j,i)= RAW(i,j+3);
  end
  XX(4,i)= RAW(i,1);
  YY(4,i)= RAW(i,4);
  COLOR(i)= RAW(i,7);
end
//
// plot
if COLOR(1)<0.
  plot2d(0,0,-1,"010"," ",[0.0,0.0,1.0,1.0])
  xfpolys(XX,YY);
else
  plot2d(0,0,-1,"010"," ",[0.0,0.0,1.0,1.0])
  xfpolys(XX,YY,COLOR);
  xfpolys(XX,YY);
end
disp('... finished');

