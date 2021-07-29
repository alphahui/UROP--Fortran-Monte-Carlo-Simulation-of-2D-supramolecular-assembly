clear all;
close all;
clc

A=dlmread('./Simulation_result/Simulation_1/SizesRawData.txt');
x=A(:,1);
y=A(:,2);
hfig=plot(x,y)
xlabel('Number of Atoms');
ylabel('Number of Islands');
print('-djpeg',"./Simulation_result/Simulation_1/CycleAtomsMovedInterval")