#include <iostream>
#include<vector>
#include <fstream>
#include <cstring>
#include <complex>
#include <sstream>
#include <stdlib.h>
#include <algorithm>
#include <functional>
#include <string>
#include <numeric>
#include <math.h>
using namespace std;


vector<vector<double> > multiplyMatrices(vector<vector<double> > A,vector<vector<double> > B,int r1,int c1,int r2,int c2)
{
    vector<vector<double> > result(r1, vector<double>(c2));

    if(c1==r2)
    {

     int i,j,k;
    for(i=0; i<r1; ++i)
        for(j=0; j<c2; ++j)
            for(k=0; k<c1; ++k)
            {
                result[i][j]+=A[i][k]*B[k][j];
            }


    }
    return result;
}


vector<vector<double> > getCofactor(vector<vector<double> > mat, vector<vector<double> > temp, int p, int q, int n)
{
    int i = 0, j = 0;
    for (int row = 0; row < n; row++)
    {
        for (int col = 0; col < n; col++)
        {
            if (row != p && col != q)
            {
                temp[i][j++] = mat[row][col];
                if (j == n - 1)
                {
                    j = 0;
                    i++;
                }
            }
        }
    }
    return temp;
}

double determinantOfMatrix(vector<vector<double> > mat, int n, int N)
{

    double D = 0;
    if (n == 1)
        return mat[0][0];

    vector<vector <double> > temp(N, vector<double>(N));
    int sign = 1;
    for (int f = 0; f < n; f++)
    {
        temp=getCofactor(mat, temp, 0, f, n);
        D += sign * mat[0][f] * determinantOfMatrix(temp, n - 1,N);
        sign = -sign;
    }

    return D;
}
vector<vector<double> > adjoint(vector<vector<double> > A,vector<vector<double> > adj, int N)
{
    if (N == 1)
    {
        adj[0][0] = 1;

    }
    else
        {

    int sign = 1;
    vector<vector <double> > temp(N, vector<double>(N));

    for (int i=0; i<N; i++)
    {
        for (int j=0; j<N; j++)
        {
            temp=getCofactor(A, temp, i, j, N);
            sign = ((i+j)%2==0)? 1: -1;
            adj[j][i] = (sign)*(determinantOfMatrix(temp, N-1,N));

        }
    }
        }
    return adj;
}


vector<vector <double> >  inverse(vector<vector <double> > A,  int N)
{
    vector<vector <double> > inverse(N, vector<double>(N));
    double det = determinantOfMatrix(A, N,N);
    if (det == 0)
    {
        cout << "Singular matrix, can't find its inverse";

    }

    vector<vector <double> >  adj(N, vector<double>(N));
    adj=adjoint(A, adj,N);

    for (int i=0; i<N; i++)
        for (int j=0; j<N; j++)
            inverse[i][j] = adj[i][j]/double(det);

    return inverse;
}

double mean(vector<double> v1){
	double sum = v1[0];
	for (unsigned int i=1; i < v1.size(); i++)
		sum += v1[i];
	return double(sum) / double(v1.size());
}

double covariance(vector<double> v1, vector<double> v2){
	double mean1 = mean(v1), mean2 = mean(v2);
	double sum = (double(v1[0]) - mean1) * (double(v2[0]) - mean2);
	for (unsigned int i=1; i < v1.size(); i++){
		sum += (double(v1[i]) - mean1) * (double(v2[i]) - mean2);
	}
	return double(sum) / double(v1.size()-1);
}

vector<double> GetAverages(vector<vector<double> > Operation)
{
    int i,j,n1,n2;
    n1=Operation.size();
    vector<double> averages=Operation.at(0);
    for(i=1;i<n1;i++)
        transform(Operation.at(i).begin(), Operation.at(i).end(), averages.begin(), averages.begin(), plus<double>());
    n2=averages.size();
    for(i=0;i<n2;i++)
        averages.at(i)=averages.at(i)/n1;//transform(averages.begin(), averages.end(), averages.begin(), bind1st(divides<float>(),10));
    return averages;
}

vector<vector<double> > GetNoiseVectors(vector<vector<double> > Operation, vector<double> M, vector<double> P)
{
    int i,j,n1,n2,n3,index;
    n1=Operation.size();
    n2=M.size();
    n3=P.size();

    vector<vector<double> > NoiseVectors;
    vector<double> temp;
    vector<double> T;
    for(j=0;j<n1;j++)
    {
        temp.clear();
        T=Operation.at(j);
        for(i=0;i<n3;i++)
        {
            index=P.at(i);
            temp.push_back(T.at(index)-M.at(index));

        }

        NoiseVectors.push_back(temp);
    }
    return NoiseVectors;
}


vector<vector<double> > GetNoiseCovarianceMatrix(vector<vector<double> > NoiseVectors)
{
    int i,j,n1,n2,n3,index;
    n1=NoiseVectors.size();
    n2=NoiseVectors.at(0).size();
    vector<vector<double> > transpose;
    vector<double > temp;
    for (i=0;i<n2;i++)
    {
        temp.clear();
        for(j=0;j<n1;j++)
        {
            temp.push_back(NoiseVectors[j][i]);
        }
        transpose.push_back(temp);
    }


    vector<vector<double> > NoiseCovMat(n2, vector<double>(n2));

    for(i=0;i<n2;i++)
    {
        for(j=0;j<n2;j++)
        {
            NoiseCovMat[i][j]=covariance(transpose.at(i),transpose.at(j));
        }
    }
    return NoiseCovMat;
}


vector<vector<double> > ReadData(char *Path)
{
    vector<vector<double> > Operation;
    vector<double> temp;
    ifstream infile(Path);
    vector<string> plain,cipher;
    string key,tempstr;
    double i;int j;
    //infile>>key; //Get key
    while(!infile.eof())
    {
        temp.clear();

        /*getline(infile,tempstr,','); //Get plain text
        plain.push_back(tempstr);
        getline(infile,tempstr,','); //Get cipher text
        cipher.push_back(tempstr);*/

        while (infile >> i)
        {
            temp.push_back(i);
            if (infile.peek() == ',')
                infile.ignore();
            if (infile.peek() == '\n')
                break;
        }
        if(!infile.eof())
            Operation.push_back(temp);
    }

    return Operation;
}

vector<vector<double> > getProbabilities(vector<vector<double> > Traces, vector<vector<vector<double> > > TemplateCovMats,
                                vector<vector<vector<double> > > TemplateInvCovMats,vector<double> TemplateDeterminants,
                                vector<vector<double> > TemplatePOIs)
{
    vector<vector<double> > probs(TemplateDeterminants.size(), vector<double>(Traces.size()));
    int i,j,p,q,N;double constant;
    for(q=0;q<TemplateDeterminants.size();q++)
    {
        vector<vector<double> > NoiseCovMat=TemplateCovMats.at(q);
        vector<vector<double> > inverseNoiseCovMat=TemplateInvCovMats.at(q);
        vector<double> POIs=TemplatePOIs.at(q);
        double determinant=TemplateDeterminants.at(q);
        constant=1/(sqrt(pow(2*M_PI,NoiseCovMat.size())*determinant));

        for(p=0;p<Traces.size();p++)
        {
            vector<vector<double> > matrix,A,B;
            vector<double> tempvec;
            double temp;
            for(i=0;i<POIs.size();i++)
                tempvec.push_back(Traces[p][POIs.at(i)]);
            A.push_back(tempvec);
            for(i=0;i<POIs.size();i++)
            {
                tempvec.clear();
                tempvec.push_back(Traces[p][POIs.at(i)]);
                B.push_back(tempvec);
            }
            matrix=multiplyMatrices(A,inverseNoiseCovMat,
                                     A.size(),A.at(0).size(),
                                     inverseNoiseCovMat.size(),inverseNoiseCovMat.at(0).size());
            matrix=multiplyMatrices(matrix,B,
                                        matrix.size(),matrix.at(0).size(),
                                        B.size(),B.at(0).size());

            temp=constant*exp(-(0.5)*matrix[0][0]);
            probs[q][p]=temp;
        }
    }
    return probs;
}


int main( )
{
    char temp[20];
    int K=2,q,i,j;
    vector<double> tempvec, M;
    double t;

    vector<vector<double> > Traces;

    vector<vector<vector<double> > > TemplateCovMats;
    vector<vector<vector<double> > > TemplateInvCovMats;
    vector<vector<double> >  TemplateMeans;
    vector<vector<double> >  TemplatePOIs;
    vector<double>  TemplateDeterminants;

    for(q=1;q<=K;q++)
    {
        vector<vector<double> > Operation;
        char Path[10]="Data";
        sprintf(temp, "%d", q);
        strcat(Path,temp);
        strcat(Path,".txt");
        Operation=ReadData(Path);
        Traces.push_back(Operation.at(0));
        M=GetAverages(Operation);
        //M=Operation.at(0);

        vector<double> P;
        double sum = accumulate(M.begin(), M.end(), 0.0);
        double mean = sum / M.size();
        double sq_sum = inner_product(M.begin(), M.end(), M.begin(), 0.0);
        double stdev = sqrt(sq_sum / M.size() - mean * mean);
        double threshupper=mean+3*stdev;        //Gaussian distribution: 99.7% of the data lies between 3*sd from mean.
        double threshlower=mean-3*stdev;

        for (j=0; j< M.size(); j++)
        {
            t=M.at(j);
            if((t>threshupper || t<threshlower))
                P.push_back(j);
        }

        vector<vector<double> > NoiseVectors=GetNoiseVectors(Operation,M,P);
        vector<vector<double> > NoiseCovMat=GetNoiseCovarianceMatrix(NoiseVectors);
        double determinant=determinantOfMatrix(NoiseCovMat,NoiseCovMat.size(),NoiseCovMat.size());
        vector<vector<double> > inverseNoiseCovMat=inverse(NoiseCovMat,NoiseCovMat.size());
        TemplatePOIs.push_back(P);
        TemplateMeans.push_back(M);
        TemplateCovMats.push_back(NoiseCovMat);
        TemplateDeterminants.push_back(determinant);
        TemplateInvCovMats.push_back(inverseNoiseCovMat);
    }

    //Get Sample Traces
    //vector<vector<double> > Traces;
    //char Path[20]="SampleData.txt";
    //Traces=ReadData(Path);
    //printf("%d",Traces.size());



    //Classification
    vector<vector<double> > probs=getProbabilities(Traces,TemplateCovMats,TemplateInvCovMats,TemplateDeterminants,TemplatePOIs);
    //Format row=trace,column=key
    vector<double> comprobs;
     for (i=0; i< probs.size(); i++)
    {
        double temp=0;
        for (j=0; j< probs.at(i).size(); j++)
            temp=temp+(double)log(probs[i][j]);
        comprobs.push_back(exp(temp));
    }
    printf("\n\nOperation Number Matched: %d",max_element(comprobs.begin(), comprobs.end()) - comprobs.begin()+1);
    cout<<endl;
    cout<<endl;
}



/*
   for (i=0; i< P.size(); i++)
        {
            cout << P.at(i)<<",";
        }
        cout<<endl;



//int jkl=NoiseCovMat.size();
        for (i=0; i< NoiseVectors.size(); i++)
        {
            tempvec=NoiseVectors.at(i);
            for (j=0; j< tempvec.size(); j++)
                    cout << tempvec.at(j)<<",";
            cout<<endl;
        }
        cout<<endl;
        cout<<endl;
        for (i=0; i< NoiseCovMat.size(); i++)
        {
            tempvec=NoiseCovMat.at(i);
            for (j=0; j< tempvec.size(); j++)
                    cout << tempvec.at(j)<<",";
            cout<<endl;
        }
        cout<<endl;
        cout<<endl;
         for (i=0; i< inverseNoiseCovMat.size(); i++)
        {
            tempvec=inverseNoiseCovMat.at(i);
            for (j=0; j< tempvec.size(); j++)
                    cout << tempvec.at(j)<<",";
            cout<<endl;
        }

        cout<<endl;
        cout<<endl;


         for (i=0; i< probs.size(); i++)
        {
                cout << probs.at(i)<<",";

        }


        cout<<endl;
        cout<<endl;
        printf("%lf",determinant);
        */

