### 2020/05/13 Keonwoo Park

### 데이터 구조론
## Quick Sort


Quick_Sort_I<-function(d){
  size_d=length(d)
  
  if(size_d<3){
    if(size_d==2 && d[1]>d[2]){
      return(c(d[2],d[1]))
    }else{
      return(d)
    }
  }
  
  left=1
  right=size_d
  pivot=d[size_d]
  
  repeat{
    while(d[left]<=pivot && left<=size_d){left=left+1}
    while(d[right]>pivot && right>=1){right=right-1}
    if(left>=right){
      break
    }
    if(left<right){
      # Swap
      temp_value=d[right]
      d[right]=d[left]
      d[left]=temp_value
    }
  }
  # Partition
  if(left==1){
    return(c(d[1],Quick_Sort_I(d[2:size_d])))
  }else if(right==size_d){
    return(c(Quick_Sort_I(d[1:(size_d-1)]),d[size_d]))
  }else{
    return(c(Quick_Sort_I(d[1:right]),Quick_Sort_I(d[(right+1):size_d])))
  }
}

Quick_Sort_D <- function(d){
  
  size_d =length(d)
  
  if(size_d<3){
    if(size_d==2 && d[1]<d[2]){
      return(c(d[2],d[1]))
    }else{
      return(d)
    }
  }
  
  left=1
  right=size_d
  pivot=d[size_d]
  
  repeat{
    while(d[left]>=pivot && left<=size_d){left=left+1}
    while(d[right]<pivot && right>=1){right=right-1}
    if(left>=right){
      break
    }
    if(left<right){
      # Swap
      temp_value=d[right]
      d[right]=d[left]
      d[left]=temp_value
    }
    
  }
  # Partition
  if(left==1){
    return(c(d[1],Quick_Sort_D(d[2:size_d])))
  }else if(right==size_d){
    return(c(Quick_Sort_D(d[1:(size_d-1)]),d[size_d]))
  }else{
    return(c(Quick_Sort_D(d[1:right]),Quick_Sort_D(d[(right+1):size_d])))
  }
}


Quick_Sort <- function(d,decreasing=F){
  if(decreasing==F){
    Quick_Sort_I(d)
  }else{
    Quick_Sort_D(d)
  }
  
}


