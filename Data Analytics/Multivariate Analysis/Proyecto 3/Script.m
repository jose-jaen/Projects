% Cargamos los datos

>> datos = readtable('coches.csv');
>> X_org = table2array(datos);

% Observamos la distribucion de las variables y sus relaciones lineales

>> plotmatrix(X_org(:,3:11))

% Aplicamos las transformaciones necesarias para aumentar la simetria

>> X = [X_org(:,1:2), log(X_org(:,3:4)), log(X_org(:,5)*0.45359237), sqrt(X_org(:,6)), log(X_org(:,7:11))];

% Vemos el efecto final

>> figure
>> plotmatrix(X(:,3:11))

% Identificamos los tipos de variables

X_cuant = X(:,3:11);
X_cual = X(:,1:2);

% Calculamos las distancias e imponemos igual variabilidad geometrica

>> M = squareform(pdist(X_cuant, 'mahal'));
>> M = M.*M;
>> M = M/vgeom(M);
>> [n, p] = size(X);
>> X3 = X_cual;

>>  for i=1:n
    for j=i:n
        alpha(i,j)=sum(X3(i,:)==X3(j,:));
        alpha(j,i)=alpha(i,j);
    end
 end

>> S_cual = alpha./p;
>> D_cual = 2*(ones(size(S_cual)) - S_cual);
>> D_cual = D_cual/vgeom(D_cual);
>> D = M + D_cual;

% Solo necesitamos el triangulo superior de la matriz de distancias

[n,p] = size(D);
for i = 1:n
    D(i,i) = 0;
end;

% Clasificación Jerárquica

>> Y = squareform(D);
>> Z_min = linkage(Y, 'single');
>> Z_max = linkage(Y, 'complete');
>> Z_UPGMA = linkage(Y, 'average');

% Visualizamos los resultados 

>> figure
>> subplot(1, 3, 1)
>> dendrogram(Z_min)
>> title('Método del Mínimo')
>> subplot(1, 3, 2)
>> dendrogram(Z_max)
>> title('Método del Máximo')
>> subplot(1, 3, 3)
>> dendrogram(Z_UPGMA)
>> title('Método UPGMA')

% Vector con correlaciones cofenéticas 

cofenetica = [cophenet(Z_min, Y); cophenet(Z_max, Y); cophenet(Z_UPGMA, Y)];

% Vemos cuál perturba más

for i = 1:size(cofenetica,1)
  if min(cofenetica)==cofenetica(i)
  disp(i)
  end;
end;

% Método que perturba menos (óptimo)

for i = 1:size(cofenetica,1)
  if max(cofenetica)==cofenetica(i)
  disp(i)
  end;
end;
figure

% Visualizamos UPGMA con nuestro threshold

dendrogram(Z_UPGMA,0,'colorthreshold',2.5)

% Tendencias y patrones con los grupos

for j = 0:1
  for i = 1:size(X,1)
    if X(i,1) == j
      disp(i)
    end;
  end;
end;

for j = 1:3
  for i = 1:size(X,2)
    if X(i,1) == j
      disp(i)
    end;
  end;
end;

% Representacion MDS

[Y,vaps,percent,acum] = coorp(D)

% Aplicamos k-means ()

[C,s,IDX]=kmedias2(Y, 6)

% Vemos interpretación de las coordenadas principales

>> correlaciones2(X, Y, 9, 1)

% Diferencias entre grupos del k-means 

>> medias=splitapply(@mean,X_cuant,IDX)
>> medianas=splitapply(@median,X_cuant,IDX)
>> mode=splitapply(@mode,X_cual,IDX)