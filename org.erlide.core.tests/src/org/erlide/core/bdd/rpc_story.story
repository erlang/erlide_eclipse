Given a backend 
When a rpc is done with args lists:reverse([1,a,8])
Then the result should be [8,a,1]
When a rpc is done with args lists:reverse([2,a,8])
Then the result should be [8,a,2]
