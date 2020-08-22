// An M-File to postprocess data written by STASL
// Visualization in 3D
// j. behrens 10/96
//
// clear old data structures
clear;
//
// open file and read data
disp('reading data ...');
RAW = read('3dinfile.dat',-1,10);
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
    ZZ(j,i)= RAW(i,j+6);
  end
  XX(4,i)= RAW(i,1);
  YY(4,i)= RAW(i,4);
  ZZ(4,i)= RAW(i,7);
  COLOR(i)= RAW(i,10);
end
//
// plot
// if COLOR(1)<0.
  plot3d(XX,YY,ZZ);
// else
//   plot2d(0,0,-1,"010"," ",[0.0,0.0,1.0,1.0])
//   xfpolys(XX,YY,COLOR);
//   xfpolys(XX,YY);
// end
disp('... finished');

