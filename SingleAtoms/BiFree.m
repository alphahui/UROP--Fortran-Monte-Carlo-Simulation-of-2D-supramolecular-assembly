%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%             Make Image from "*.txt" files
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clc;clear;

MX=[1:400];                                                                %x-axis
MY=[3^(1/2):3^(1/2):100*3^(1/2)];                                %y-axis

startdata=1
interval=195
finaldata=3901
fileLocation ='.\Simulation_result\Simulation_1\'
% aviname='Brsix.avi';

%read the .txt from the folder
for i=startdata:interval:finaldata;
    if isfile([fileLocation,num2str(i),'.txt'])
    load([fileLocation,num2str(i),'.txt']);
    end
end

%change .txt into image and save as .jpg
for i=startdata:interval:finaldata;
    if isfile([fileLocation,num2str(i),'.txt'])
        str=['X',num2str(i)];
A=eval(str);
    
[m,n]=size(A);                                                                      %calculate the size of matrix
figure
hold on
markersizeA=round(260*sqrt(100/(m*n)));  %260 150 for large 20 for small
markersizeB=round(260*sqrt(100/(m*n)));                            %background

%**************************************************************************

[q,p]=find(A==0);
pp=p*2+q-2;                                                        %function to calculate relationship
plot(MX(pp),MY(q),'.','Color',[105/255,105/255,105/255],...        %'.' means dot, 'color' define gray color
                  'markersize',markersizeB)                        %define the dot size

[q,p]=find(A==1);
pp=p*2+q-2;
plot(MX(pp),MY(q),'.','Color',[255/255,150/255,0/255],...             %orange
                  'markersize',markersizeA)  
                           
[q,p]=find(A==2);
pp=p*2+q-2;
plot(MX(pp),MY(q),'.','Color',[105/255,105/255,105/255],...         %orange
                  'markersize',markersizeA)
                
[q,p]=find(A==3);
pp=p*2+q-2;
plot(MX(pp),MY(q),'.','Color',[255/255,255/255,0/255],...            %yellow
                  'markersize',markersizeA)

[q,p]=find(A==4);
pp=p*2+q-2;
plot(MX(pp),MY(q),'.','Color',[255/255,150/255,0/255],...            %orange
                  'markersize',markersizeA)              

[q,p]=find(A==5);
pp=p*2+q-2;
plot(MX(pp),MY(q),'.','Color',[255/255,255/255,0/255],...            %yellow
                  'markersize',markersizeA)              
%**************************************************************************

axis([0 180 0 100])                                       %make sure the white are almost same

axis off;

set (gcf,'Position',[100,100,1500,700]);                    %300,100 is the ordination; 1000,600 is the height and width for window   

set(gca,'LooseInset',get(gca,'TightInset'))                 %Tight in the page

set (gcf,'PaperPositionMode','auto')                       %for save, otherwise save image will change the size
print('-djpeg',[fileLocation,num2str(i)])

close all
    end


end

%change .jpg into a moive
% aviobj = avifile(aviname);
% aviobj.Quality = 100;
% aviobj.fps=4;
% aviobj.compression='None';
% 
% for i=0:interval:finaldata;
% 	frame=im2frame(imread(strcat(num2str(i),'.jpg')));
% 	aviobj = addframe(aviobj,frame);
% end
% 
% aviobj = close(aviobj);












