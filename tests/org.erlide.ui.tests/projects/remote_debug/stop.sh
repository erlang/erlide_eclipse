ps -ef | grep qvladum | grep remote_ | grep beam | cut -d " " -f 3 | xargs kill
