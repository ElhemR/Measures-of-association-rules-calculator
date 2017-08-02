associationRules=function(filedata,filerule,dest,symbolrule){
dataset<-read.table(file = filedata, sep=",");
nrows<-dim(dataset)[1];
ncols<-dim(dataset)[2];
rulevect<-as.vector(datarule);
antecedents<-c();
consequents<-c();
dataline<-c();
conviction<--1;
nboc<-0;
nbant<-0;
nbcons<-0;
l<-0;
colres<-c();
nbrules<-length(readLines(file(filerule)));
resultat<-matrix(ncol = 50,nrow = nbrules);
while(l<nbrules){
  datarule<-read.table(file=filerule, sep="",nrows=1,skip=l);
  rulevect<-as.vector(datarule);
  colres[length(colres)+1]<-paste(as.matrix(datarule), collapse = " ");
  a<-1;
  while(rulevect[a]!=symbolrule){
    antecedents[length(antecedents)+1]=as.character(unlist(rulevect[a]));
    a<-a+1;
  }
  a<-a+1;
  while(a<=length(rulevect)){
    consequents[length(consequents)+1]=as.character(unlist(rulevect[a]));
    a<-a+1;
  }
  for(i in 1:nrows){
    
    for(j in 1:ncols){
      dataline[length(dataline)+1]=as.character(unlist(dataset[i,j]));
      
    }
    ant<-intersect(antecedents,dataline);
    cons<-intersect(consequents,dataline);
    v<-union(ant,cons);
    if(length(v)==length(union(antecedents,consequents))) nboc<-nboc+1;
    if(length(ant)==length(antecedents)) nbant<-nbant+1;
    if(length(cons)==length(consequents)) nbcons<-nbcons+1;
    dataline<-c();
    support<-nboc/nrows;
    confidence<-nboc/nbant;
    pa<-nbant/nrows;
    pc<-nbcons/nrows;
    pac<-support/pc;
    pca<-support/pa;
    pnotanotc<-1-((nboc+nbant+nbcons)/nrows);
    pnotac<-pc-support;
    panotc<-pa-support;
    pnotcka<-panotc/pa;
    pnotcknota<-pnotanotc/(1-pa);
    pnotaknotc<-pnotanotc/(1-pc);
    pcknota<-pnotac/(1-pa);
    x<-log(support/(pa*pc));
    x1<-log(pnotanotc/(1-pa)*(1-pc));
    x2<-log(panotc/(pa*(1-pc)));
    x3<-log(pnotac/((1-pa)*pc));
    
    
    
    
    lift<-support/(pa*pc);
    leverage<-confidence-(pa*pc);
    accuracy<-support+pnotanotc;
    conviction<-(1-pc)/(1-confidence);
    onewaysup<-pca*x;
    twowaysup<-support*x;
    twowaysupvar<-twowaysup+pnotanotc*x1+panotc*x2+pnotac*x3;
    addedvalue<-pca-pc;
    certaintyfac<-(pac-pc)/(1-pc);
    corr<-(pac-pa*pc/sqrt(pa*pc*(1-pa)*(1-pc)));
    chi2<-nrows*(corr^2);
    n1<-min(min(pa,(1-pa)),min(pc,(1-pc)));
    n2<-min(max(pa,(1-pa)),max(pc,(1-pc)));
    gan<-2*pca-1;
    lmax<-((n1*n2)^2*nrows)/(pa*(1-pa)*pc*(1-pc));
    dchi2<-sqrt(nrows/lmax)*chi2;
   ccr1<-nboc*(nrows*(panotc+pnotac));
    ccr2<-(panotc*nrows)*(nboc+(pnotac*nrows));
    ccr<-ccr1/ccr2;
    collstr<-((support+pnotcknota)/(pa*pc+(1-pa)*(1-pc)))*((1-pa*pc-(1-pa)*(1-pc))/(1-support-pnotcknota));
    ccs<-(nrows-nbcons)/(panotc*nrows);
    confc<-0.5*((support/pa)+(pnotanotc/(1-pc)));
    confirmc<-support+pnotanotc-2*panotc;
    confirmd<-support-panotc;
    ccc<-0.5*(pca+pnotaknotc)-pnotcka;
    ccd<-pca-pnotcka;
    cosine<-support/sqrt(pa*pc);
    excex<-1-(panotc/support);
    fm<-2*pca*pac/(pca+pac);
    gini<-pa*(pca^2+pnotcka^2)+(1-pa)*(pcknota^2+pnotcknota^2)-pc^2-(1-pc)^2;
    gk<-sum(max(pac,pnotac),max(panotc,pnotac))+sum(max(pnotanotc,pnotac),max(panotc,pac))-max(pa,(1-pa))-max(pc,(1-pc));
    hconf<-0;
     for(i in 0:nboc-1){
      hconf<-hconf+(choose(nbcons,i)*choose(nrows-nbcons,nbant-i))/choose(nboc,nbant);
    }
   impind<-(sqrt(nrows))*(pa*(1-pc)-panotc/sqrt(pa*(1-pc)));
 infgain<-log(pac/pa*pc);
   nanotc<-panotc*nrows;
   ld<-nbant*(nrows-nbcons)/nrows;

   sum<-0;
   for(i in 0:nanotc){
     sum<-sum+ld^i/factorial(i)*exp(-ld);
   }
   
   intimp<-1-sum;
    iwd<-(exp(log(infgain))-1)*pac;
    jacc<-pac/(pa+pc-pac);
   jm<-(pac*log(pca/pc))-(panotc*log(pnotcka/(1-pc)));
   kappa<-(pac+pnotanotc-pa*pc-(1-pa)*(1-pc))/(1-pa*pc-(1-pa)*(1-pc));
   klos<-sqrt(pac)*(pca-pc);
   km<-pca*log(pca/pc)+(1-pca)*log((1-pca)/(1-pc))-pca*log(pca/(1-pc))-(1-pca)*log((1-pca)/pc);
   lap<-(nboc+1)/(nbant+2);
   lc<-(pac-panotc)/pc;
   loe<-(pa*(1-pc))/panotc-1;
   oddmul<-(pac*(1-pc))/(panotc*pc);
   oddrat<-(pac*pnotanotc)/(panotc*pnotac);
   ps<-pac-(pa*pc);
   locsup<-pac/pc;
   relrisk<-pca/pcknota;
   ss<-pac/panotc;
   spec<-pnotcknota;
   glbsupp<-pac;
   zhang<-ps/max(pac*(1-pc),panotc*pc);
   
   
   
    resultat[l+1,1]<-support;
    resultat[l+1,2]<-confidence;
    resultat[l+1,3]<-lift;
    resultat[l+1,4]<-leverage;
    resultat[l+1,5]<-accuracy;
    resultat[l+1,6]<-conviction;
    resultat[l+1,7]<-onewaysup;
    resultat[l+1,8]<-twowaysup;
    resultat[l+1,9]<-addedvalue;
    resultat[l+1,10]<-certaintyfac;
    resultat[l+1,11]<-twowaysupvar;
    resultat[l+1,12]<-chi2;
    resultat[l+1,13]<-ccr;
    resultat[l+1,14]<-collstr;
    resultat[l+1,15]<-ccs;
    resultat[l+1,16]<-confc;
    resultat[l+1,17]<-confirmc;
    resultat[l+1,18]<-confirmd;
    resultat[l+1,19]<-ccc;
    resultat[l+1,20]<-ccd;
    resultat[l+1,21]<-corr;
    resultat[l+1,22]<-cosine;
    resultat[l+1,23]<-dchi2;
    resultat[l+1,24]<-excex;
    resultat[l+1,25]<-fm;
    resultat[l+1,26]<-gan;
    resultat[l+1,27]<-gini;
    resultat[l+1,28]<-gk;
    resultat[l+1,29]<-hconf;
    resultat[l+1,30]<-impind;
   resultat[l+1,31]<-infgain;
   resultat[l+1,32]<-intimp;
   resultat[l+1,33]<-iwd;
   resultat[l+1,34]<-jacc;
   resultat[l+1,35]<-jm;
   resultat[l+1,36]<-kappa;
   resultat[l+1,37]<-klos;
   resultat[l+1,38]<-km;
   resultat[l+1,39]<-lap;
   resultat[l+1,40]<-lc;
   resultat[l+1,41]<-loe;
   resultat[l+1,42]<-oddmul;
   resultat[l+1,43]<-oddrat;
   resultat[l+1,44]<-ps;
   resultat[l+1,45]<-locsup;
   resultat[l+1,46]<-relrisk;
   resultat[l+1,47]<-ss;
   resultat[l+1,48]<-spec;
   resultat[l+1,49]<-glbsupp;
   resultat[l+1,50]<-zhang;
  }
  nbant<-0;
  nboc<-0;
  nbcons<-0;
  antecedents<-c();
  consequents<-c();
  rulevect<-c();
  l<-l+1;
}
colnames(resultat)<-c("Support","Confidence","Lift","Leverage","Accuracy","Conviction","1-way support","2-way support","Added value","Certainty factor","2-way support variation","Chi-square","Class correlation ratio","Collective strength","Complement class support","Confidence causal","Confirm causal","Confirm descriptive","Confirmed-confidence causal","Confirmed-confidence descriptive","Correlation coefficient","Cosine/IS","Dilated chi-square","Example and counterexample rate","F-measure","Ganascia","Gini","Goodman-Kruskal","Hyper confidence","Implication index","Information gain","Intensity of implication","Interestingness Weighting Dependency","Jaccard","J-measure","Kappa","Klosgen","K-measure","Laplace correlation","Least contradiction","Loevinger","Odd multiplier","Odds ratio","Piatetsky-Shapiro","Recall/local support","Relative risk","Sebag-Schoenauer","Specificity","Support/global support","Zhang");
rownames(resultat)<-colres;
write.csv(x=resultat,file=dest);

}


















