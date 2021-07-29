clear all;
close all;
clc


document = '.\report\Week 5\simulation\added per cycle\0.1 per cycle\FinalSizesRawData';
docType='.txt';
docIn= append(document,docType);


A=dlmread(docIn);
x=A(:,1);
y=A(:,2);
hfig=plot(x,y);
xlabel('sizes of island');
ylabel('number of island');
print('-djpeg',document)