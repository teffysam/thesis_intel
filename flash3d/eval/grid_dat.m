% An M-File to postprocess data written by STASL
% Visualization in 3D
% j. behrens 6/94
%
% open file and read data
disp('reading data ...');
iou = fopen('Flash90_matlab.0000');
LEN = fscanf(iou,'%d',1)
LAV = fscanf(iou,'%f',[LEN,LEN]);
stat = fclose(iou);
disp('... done, now processing grid ...');
%
% transpose
VAL= LAV.';
%
% plot contours
contourf(VAL);
disp('... finished');

