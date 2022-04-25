function corr_table=correlacions(X,Y,pcuant,pnominal)
p=size(X,2); pordinal=p-pcuant-pnominal;
corr_table=zeros(p,2);

corr_table(1:pcuant,:)=corr(Y(:,1:2),X(:,1:pcuant),'type','Pearson')';

for i=pcuant+1:pcuant+pnominal
    corr_table(i,:)=[CramerV(X(:,i),Y(:,1)) CramerV(X(:,i),Y(:,2))];
end

corr_table(pcuant+pnominal+1:p,:)=corr(Y(:,1:2),X(:,pcuant+pnominal+1:p),'type','Spearman')';


L1={'Y_1','Y_2'};

MM=corr_table;
dd=size(MM);
mymap=[0 0 0.8
0 0.4 1
0.4 0.8 1
1 1 1
1 0.6 1
1 0.2 0.8
0.5 0 0.5];
colormap(mymap);

clims=[-1 1];
imagesc(MM,clims); 
set(gca, 'XTick', 1:dd(2)); 
set(gca, 'YTick', 1:dd(1)); 
set(gca, 'XTickLabel', L1); 

title('Mapa de Calor Coordenadas Principales', 'FontSize', 12); 
colorbar;
end