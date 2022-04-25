% Cargamos la base de datos

>> datos = readtable('coches.csv');

% Observamos la estructura de los datos

>> datos(1:10, :)

% Seleccionamos las variables cuantitativas

>> X_cuant = table2array(datos(:, 3:11));
>> X_cuant(:, 5) = X_cuant(:, 5)*0.45359237;
>> X_cuant(:, 8) = X_cuant(:, 8)/10;
>> X_cuant(:, 9) = X_cuant(:, 9)/100;

% Calculamos indice escalar de correlacion

>> format long
>> 1 - det(corr(X_cuant)) % eta^2

% Estudiamos diferencias entre varianzas

>> var(X_cuant, 1)

% Realizamos PCA 

>> [T1,Y1,acum1,T2,Y2,acum2] = PCA(X_cuant);

% Elegimos tres Componentes Principales

>> T2(:, 1:3)

% Grafico 3D

>> [coeff,score,latent,tsquared,explained] = pca(X_cuant, 'VariableWeights','variance');
>> scatter3(score(:, 1),score(:, 2),score(:, 3))
axis equal
xlabel('1a  Componente Principal')
ylabel('2a  Componente Principal')
zlabel('3a  Componente Principal')

% Vemos correlaciones con variables originales
>> corr(Y2(:, 1), X_cuant)'                            
>> corr(Y2(:, 2), X_cuant)'                             
>> corr(Y2(:, 3), X_cuant)'   

% Aunamos variables cualitativas                  

>> X_cual = table2array(datos(:, 1:2));

% Matriz de datos conjunta

>> X = [X_cuant X_cual];

% Calculamos distancias estadisticas

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

% Realizamos MDS

>> [Y,vaps,percent,acum] = coorp(D);

% Correlaciones con variables originales

>> corr_table = correlacions(X, Y, 9, 2);

% Identificacion de perfiles

>> identif_cuantis(X_cuant, Y)
>> identif_cualis(X_bin, Y)

% Correccion de correlaciones por juntar binaria y multiestado

>> corr_table = correlacions(X, Y, 9, 2);
>> corr(Y(:,1:3),X(:,11),'type','Spearman')'                    