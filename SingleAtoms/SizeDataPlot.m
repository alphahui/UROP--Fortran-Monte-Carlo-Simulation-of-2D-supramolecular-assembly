clear all;
close all;
clc


document = './report/Week 7/Change in Ed/ed 0.7/FinalSizesRawData';
docType='.txt';
docIn= append(document,docType);


A=dlmread(docIn);
x=A(:,1);
y=A(:,2);
hfig=plot(x,y);
xlabel('sizes of island');
ylabel('number of island');
print('-djpeg',document)