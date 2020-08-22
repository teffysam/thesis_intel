% read lon,lat,dep

lon_ts=ncread('agulhas_grid.cdf','LON_TS');
lat_ts=ncread('agulhas_grid.cdf','LAT_TS');
lon_u=ncread('agulhas_grid.cdf','LON_U');
lat_u=ncread('agulhas_grid.cdf','LAT_U');
lon_v=ncread('agulhas_grid.cdf','LON_V');
lat_v=ncread('agulhas_grid.cdf','LAT_V');
lon_w=ncread('agulhas_grid.cdf','LON_TS');
lat_w=ncread('agulhas_grid.cdf','LAT_TS');

dep=ncread('agulhas_salt.cdf','AX008');

% read variables for all timesteps (large! do one at a time)

%salt=ones(1101,501,23,1096);
%theta=ones(1101,501,23,1096);
uvel=ones(1101,501,23,5);
vvel=ones(1101,501,23,5);
wvel=ones(1101,501,23,5);

for n=1:5

%salt(:,:,:,n)=squeeze(ncread('agulhas_salt.cdf','SALT',[1 1 1 n],[inf inf inf 1]));
%theta(:,:,:,n)=squeeze(ncread('agulhas_theta.cdf','THETA',[1 1 1 n],[inf inf inf 1]));
uvel(:,:,:,n)=squeeze(ncread('flowx.nc','UVEL',[1 1 1 n],[inf inf inf 1]));
vvel(:,:,:,n)=squeeze(ncread('flowy.nc','VVEL',[1 1 1 n],[inf inf inf 1]));
wvel(:,:,:,n)=squeeze(ncread('flowz.nc','WVEL',[1 1 1 n],[inf inf inf 1]));

end

