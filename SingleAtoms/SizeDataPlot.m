clear all;
close all;
clc


document = '.\Simulation_result\Simulation_1\NumberOfScatterData';
docType='.txt';
docIn= append(document,docType);


A=dlmread(docIn);
x=A(:,1);
y=A(:,2);
hfig=plot(x,y);
xlabel('cycle');
ylabel('Number Of Scatter');
print('-djpeg',document)