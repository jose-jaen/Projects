datos = readtable('coches.csv');
cuantis = table2array(datos(:, [3:11]));

% Gráfico de dispersión matricial

plotmatrix(cuantis)

% Análisis multivariante exploratorio

mean(cuantis)
S = cov(cuantis, 1)
R = corr(cuantis)
var_gen = det(S)
var_total = trace(S)
eta_square = 1 - det(R)

% Los nuevos datos son las variables transformadas en Python

datos = readtable('coches_new.csv');
cuantis = table2array(datos(:, [3:11]));

% Gráfico de dispersión matricial en las nuevas variables

plotmatrix(cuantis)

% Análisis multivariante exploratorio en las nuevas variables

mean(cuantis)
S = cov(cuantis, 1)
R = corr(cuantis, 1)
var_gen = det(S)
var_total = det(S)
eta_square = 1 - det(R)

% Análisis Variable Binaria

combustible = table2array([datos(:, [1])]);
gplotmatrix(cuantis, [], combustible, 'br')

gasolina_posicion = find(combustible == 1);
diesel_posicion = find(combustible == 0);
p = 9; n_gasolina = 181; n_diesel = 20;
gasolina = [cuantis(gasolina_posicion',:)];
diesel = [cuantis(diesel_posicion',:)];

% Preparamos contraste de hipótesis T^2 de Hotelling

[m_gas, S_gas, R_gas] = stats(gasolina); [m_diesel, S_diesel, R_diesel] = stats(diesel);
Sp = ((1/(n_gasolina + n_diesel))*(n_gasolina*S_gas + n_diesel*S_diesel));
diferencia = m_gas'-m_diesel';
f = (n_gasolina+n_diesel-p-1)/(p*(n_gasolina + n_diesel-2))*(n_gasolina*n_diesel)/(n_gasolina + n_diesel)*diferencia'*inv(Sp)*diferencia;
df1 = p; df2 = n_gasolina + n_diesel - p - 1;
cv = finv(0.95, df1, df2);

% Análisis Variable Multiestado

motriz = table2array(datos(:, 2));
gplotmatrix(cuantis,[], motriz)
tras = find(motriz == 1); del = find(motriz == 2); cuatro = find(motriz == 3);
tras = [cuantis((tras)',:)]; del = [cuantis((del)',:)]; cuatro = [cuantis((cuatro)',:)];

% Preparamos contraste de hipótesis Lambda de Wilks

[m_tras, S_tras, R_tras] = stats(tras);
[m_del, S_del, R_del] = stats(del);
[m_cuatro, S_cuatro, R_cuatro] = stats(cuatro);
n_tras = 75; n_del = 118; n_cuatro = 8;
dif_tras = m_tras' - mean(cuantis)'; dif_del = m_del' - mean(cuantis)'; dif_cuatro = m_cuatro' - mean(cuantis)';
B_tras = n_tras*dif_tras*dif_tras'; B_del = n_del*dif_del*dif_del'; B_cuatro = n_cuatro*dif_cuatro*dif_cuatro';
B = B_tras + B_del + B_cuatro;
W_tras = n_tras*S_tras; W_del = n_del*S_del; W_cuatro = n_cuatro*S_cuatro;
W = W_tras + W_del + W_cuatro;
Disp_total = B + W;
est = (det(W)/(det(W + B)));
a = 201 - 3; b = 3-1 ; p = 9;
alpha = a + b-(p + b + 1)/2; beta = sqrt(((p^2)*(b^2) - 4)/((p^2) + (b^2) - 5));
lambda = (p*b-2)/4;
f = ((1-(est^(1/beta)))/((est^(1/beta))))*(((alpha*beta) - 2*lambda)/(p*b));
df1 = p*b; df2 = alpha*beta - 2*lambda;
cv = finv(0.95, df1, df2)
