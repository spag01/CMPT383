#include <bits/stdc++.h>
using namespace std;
set<string> getStrings(string line){
	string result="";
	int i=0;
	set<string> ballots;
	for(i=0;i<line.length();i++){
		if(line[i]==' '){
			ballots.insert(result);
			result="";
		}
		else if(!(line[i]>='a' && line[i]<='z') && !(line[i]>='A' && line[i]<='Z')){
			continue;
		}
		else{
			result+=line[i];
		}
	}

	if(result.length()>0)
		ballots.insert(result);

	return ballots;
}
int main()
{
	cout<<"What is the name of ballot file?"<<endl;
	string nameOfBallotFile;
	cin>>nameOfBallotFile;
    string line;
  	ifstream ballotFile (nameOfBallotFile);
  	if (ballotFile.is_open())
  	{
  		map<string,int> mp;
  		vector<int> sizeOfBallots;
  		int emptyBallots = 0;
  		int fullBallots = 0;
    	while (getline (ballotFile,line))
    	{	
    		if(line != "none"){
    		    set<string> ballots = getStrings(line);
    			for(auto it : ballots){
    				mp[it]++;
    			} 
    			sizeOfBallots.push_back(ballots.size());
    		}
    		else{
    			sizeOfBallots.push_back(0);
    			emptyBallots++;
    		}
    	}

    	for(auto it : sizeOfBallots){
    		if(it == mp.size())
    			fullBallots++;
    	}

    	vector<pair<int,string>> ballots;
    	cout<<endl;
    	cout<<"Total # of ballots: "<<sizeOfBallots.size()<<endl;
    	cout<<endl;
    	for(auto it : mp){
    		ballots.push_back({it.second,it.first});
    	}
    	sort(ballots.begin(),ballots.end(),greater<pair<int,string>>());
    	for(auto it : ballots){
    		cout<<it.second<<": "<<it.first<<endl;
    	}
    	cout<<endl;
    	cout<<"empty: "<<emptyBallots<<endl;
    	cout<<"full: "<<fullBallots<<endl;
    	ballotFile.close();
  	}
	else 
		cout << "Unable to open file"; 

    return 0;
}