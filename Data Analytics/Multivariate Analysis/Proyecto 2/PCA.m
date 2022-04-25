function [T1,Y1,acum1,T2,Y2,acum2] = PCA(X)
[n,p]=size(X);

for i=1:n
  lab(i,:)=sprintf('%3g',i);
end

H=eye(n)-ones(n)/n;
X=H*X;

S=cov(X,1);
R=corr(X);

[T1,D1]=eigsort(S);

if ((sum(sign(T1(:,1))) < 0) & (sum(sign(T1(:,2))) < 0))
  T1=-T1;
end
s=sum(D1(1:p));
for i=1:p
  percent1(i)=(D1(i)/s)*100;
  acum1(i)=sum(percent1(1:i));
end

[T2,D2]=eigsort(R);

if ((sum(sign(T2(:,1))) < 0) & (sum(sign(T2(:,2))) < 0))
  T2=-T2;
end
for i=1:p
  percent2(i)=(D2(i)/p)*100;
  acum2(i)=sum(percent2(1:i));
end
s=diag(sqrt(diag(S)));
T2 = T2*inv(s);

Y1=X*T1;
subplot(2,2,1); 
plot(Y1(:,1),Y1(:,2),'.b','MarkerSize',15)
grid
xlabel('1a. Componente Principal','FontSize',10)
ylabel('2a. C.P.','FontSize',10)
title(['A.C.P. a partir de S  (',num2str(acum1(2)),'%)'],'FontSize',10)
if n<=100
   for i=1:n
     text(Y1(i,1),Y1(i,2),lab(i,:));
   end
end   
subplot(2,2,2);
plot(D1,'o-b','LineWidth',2)
hold on
plot(mean(D1)*ones(1,p),'-r','LineWidth',2)
plot(0.7*mean(D1)*ones(1,p),'--r','LineWidth',2)
xlabel('valores propios','FontSize',10)
title(['Scree-plot y Criterio de Kaisser (S)'],'FontSize',10)
axis([1 p 0 D1(1)])

Y2=X*inv(s)*T2;
subplot(2,2,3); 
plot(Y2(:,1),Y2(:,2),'.b','MarkerSize',15)
grid
xlabel('1a  Componente Principal','FontSize',10)
ylabel('2a. C.P.','FontSize',10)
title(['A.C.P. a partir de R  (',num2str(acum2(2)),'%)'],'FontSize',10)
if n<=100
   for i=1:n
     text(Y2(i,1),Y2(i,2),lab(i,:));
   end
end   
subplot(2,2,4);
plot(D2,'o-b','LineWidth',2)
hold on
plot(ones(1,p),'-r','LineWidth',2)
plot(0.7*ones(1,p),'--r','LineWidth',2)
xlabel('valores propios','FontSize',10)
title(['Scree-plot y Criterio de Kaisser (R)'],'FontSize',10)
axis([1 p 0 D2(1)])
end