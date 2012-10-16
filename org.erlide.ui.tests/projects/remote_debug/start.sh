erl -detached -noinput -name remote_debug_slave@127.0.0.1 -setcookie erlide -pa ebin	
sleep 2 
erl -detached -noinput -name remote_debug@127.0.0.1 -setcookie erlide -pa ebin	-s start setup

