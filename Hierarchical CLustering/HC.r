
#Task 1:
distance = function(x)
{
  x = as.matrix(x)
  u = apply(x*x,1,sum) %*% matrix(1.0,1,nrow(x))
  sqrt(abs(u + t(u) - 2 * x %*% t(x)))
}

ordering = function(mim)
{
  Non = nrow(mim) + 1
  ordering = rep(0,Non)
  ordering[1] = m[Non-1,1]
  ordering[2] = m[Non-1,2]
  location = 2
  for(i in seq(Non-2,1))
  {
    for(j in seq(1,location))
    {
      if(ordering[j] == i)
      {
        ordering[j] = mim[i,1]
        if(j==location)
        {
          location = location + 1
          ordering[location] = mim[i,2]
        } else
        {
          location = location + 1
          for(k in seq(loc, j+2)) ordering[k] = ordering[k-1]
          ordering[j+1] = mim[i,2]
        }
      }
    }
  }
  -ordering
}

hi_clust = function(d, method=c("single","complete","average","centroid"))
{
  if(!is.matrix(d)) d = as.matrix(d)
  
  method_function = switch(match.arg(method),
                     single   = min,
                     complete = max,
                     average  = mean,
                     centroid  = median)
  N = nrow(d)
  diag(d)=Inf
  n = -(1:N)                       
  m = matrix(0,nrow=N-1, ncol=2)   
  h = rep(0,N-1)                   
  for(j in seq(1,N-1))
  {
   
    h[j] = min(d)
    i = which(d - h[j] == 0, arr.ind=TRUE)
    i = i[1,,drop=FALSE]
    p = n[i]
    p = p[order(p)]
    m[j,] = p
    grp = c(i, which(n %in% n[i[1,n[i]>0]]))
    n[grp] = j
    r = apply(d[i,],2,method_function)
    d[min(i),] = d[,min(i)] = r
    d[min(i),min(i)]        = Inf
    d[max(i),] = d[,max(i)] = Inf
  }
  
  structure(list(merge = m, height = h, order = iorder(m),
                 labels = rownames(d), method = method, 
                 call = match.call(), dist.method = "euclidean"), 
            class = "hclust")
}

i=seq(1,by=3,length.out=50)
x=as.matrix(iris[i,1:4])
h=hclust(dist(x),method="single")
h1=hi_clust(dist(x),method="single")
#print(cbind(h$merge[1:22,],h1$merge[1:22,]))
plot(h1)

h=hclust(dist(x),method="complete")
h2=hi_clust(dist(x),method="complete")
#print(cbind(h$merge[1:22,],h1$merge[1:22,]))
plot(h2)

h=hclust(dist(x),method="average")
h3=hi_clust(dist(x),method="average")
#print(cbind(h$merge[1:22,],h1$merge[1:22,]))
plot(h3)


h=hclust(dist(x),method="centroid")
h4 = hi_clust(dist(x),method="centroid")
#print(cbind(h$merge[1:22,],h1$merge[1:22,]))
plot(h4)



#Task 2:
#loading the dataset
file_data <- read.delim("nci.data.txt", header=FALSE, sep=" ")
t(file_data)
file_trans <- t(file_data)
file_usable <- file_trans[complete.cases(file_trans), ]

#Task 3:
#K-means clustering
#The kmeans() function performs K-means clustering in R.



#K = 1
set.seed(1)
km.out <- kmeans(file_trans, 1,nstart = 20)
km.out$cluster


#K = 2
set.seed(1)
km.out = kmeans(file_trans, 2, nstart=20)
km.out$cluster

#K = 3
set.seed(2)
km.out = kmeans(file_trans, 2, nstart=20)
km.out$cluster

#K = 4
set.seed(2)
km.out = kmeans(file_trans, 2, nstart=20)
km.out$cluster
