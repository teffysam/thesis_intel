% An M-File to postprocess data written by STASL
% Visualization in 3D
% j. behrens 10/96
%
% clear old data structures and initialize
clear;
  colormap(hsv)
  brighten(0.8)
%
% open file and read data
disp('reading data ...');
iou = fopen('3dinfile.dat');
RAW = fscanf(iou,'%f %f %f %f %f %f %f %f %f %d',[10,inf]);
stat = fclose(iou);
disp('... done, now processing grid ...');
%
% determine sizes
m= size(RAW,2);
%
% loop through elements
for i=1:m,
  for j=1:3,
    XX(j,i)= RAW(j,i);
    YY(j,i)= RAW(j+3,i);
    ZZ(j,i)= RAW(j+6,i);
  end
  XX(4,i)= RAW(1,i);
  YY(4,i)= RAW(4,i);
  ZZ(4,i)= RAW(7,i);
  COLOR(i)= RAW(10,i);
end
%
% plot
%if COLOR(1)<0
  plot3(XX,YY,ZZ,'b');
%else
%  fill3(XX,YY,ZZ,COLOR);
%end
xlabel('x-Achse');
ylabel('y-Achse');
zlabel('z-Achse');
hidden off;
grid on;
rotate3d on;
disp('... finished');

