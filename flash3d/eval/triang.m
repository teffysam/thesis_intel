%% An M-File to postprocess data
%%
%% Aufruf z.B. triang(5) : visualisiert das ergebnis des 5.ten
%% zeitschritts (vorausgesetzt nat"urlich, die files Flash90_matval.0005
%% und Flash90_mattri.0005 wurden mit matlab_tri_plot (im modul IO_plot) 
%% erzeugt).  
%%
%% N. Rakowsky 5/99
%
%
function triang(zeitschritt)
%
%% knoten-koordinaten und tracer-konzentration
file_laden = ['load Flash90_matval.',sprintf('%04i',zeitschritt)];
eval(file_laden)
%% triangulierung: per element die 3 zugeh"origen knoten 
%% (zeiger auf zeilen in Flash90_matval)
file_laden = ['load Flash90_mattri.',sprintf('%04i',zeitschritt)];
eval(file_laden)
%
tri = Flash90_mattri;
[d,hilf]=size(Flash90_matval);
x=Flash90_matval(1:d,1);
y=Flash90_matval(1:d,2);
z=Flash90_matval(1:d,3);
%
%% und 3D-plot
trisurf(tri,x,y,z)
%
%% J"orns colormap:
cj(1:64,1) = linspace(0,1,64)';
cj(1:64,2) = zeros(64,1);
cj(1:64,3) = linspace(1,0,64)';
colormap(cj)
%
%
%% grid off:
%shading interp
%
%% 2D-plot
%view(2)
%axis equal
%axis([-.5 .5 -.5 .5])
%
%% zoomen, ausschnitt x1<x<x2, y1<y<y2
%% axis([x1 x2 y1 y2])
%axis([-.4 0 -.4 0]) 
%
%
%% interpolieren auf regelm"assiges gitter, z.b. f"ur schnitte
%[xi,yi]=meshgrid(-.5:.01:.5);
%zi=griddata(x,y,z,xi,yi);
%mesh(xi,yi,zi)
