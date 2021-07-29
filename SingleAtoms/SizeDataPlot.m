clear all;
close all;
clc


document = './Simulation_result/Simulation_2/CycleAtomsMovedInterval';
docType='.txt';
docIn= append(document,docType);


A=dlmread(docIn);
x=A(:,1);
y=A(:,2);
hfig=plot(x,y);
xlabel('The time it moved');
ylabel('Time took to move');
print('-djpeg',document)