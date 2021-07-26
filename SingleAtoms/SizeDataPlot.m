clear all;
close all;
clc

A=dlmread('FinalSizesRawData.txt');
x=A(:,1);
y=A(:,2);
hfig=plot(x,y)
xlabel('Number of Atoms');
ylabel('Number of Islands');
