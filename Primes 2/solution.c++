#include <bits/stdc++.h>
#define llg long long
using namespace std;
llg a, b;
set<llg> primos;
bool EsCompuesto[31623];
bool IsCompound[5000005];
void Cribar()
{
    for(llg i = 2; i < 31623; i++)
    {
        if (!EsCompuesto[i])primos.insert(i);
        for (llg j = i, aux; (aux = j*i) <= 31623; j++)
            EsCompuesto[aux] = true;
    }
}
int main()
{
    Cribar();
    scanf("%lld", &a);
    scanf("%lld", &b);
    for (llg i = 0; i <= b - a; i++)
        if (IsCompound[i] == false)
        {
            bool flag = false;
            for (auto p : primos)
            {
                llg root = sqrt(i + a);
                if (p > root)break;
                if ((flag = ((((i + a) % p) == 0) && (p != i + a))) == true)
                {
                    for (llg k = 0, aux; (aux = k*p + i + a) <= b; k++)
                        IsCompound[aux - a] = true;
                    primos.erase(p);
                    break;
                }
            }
            if ((!flag) && (a + i > 1))printf("%lld\n", a + i);
        }
}