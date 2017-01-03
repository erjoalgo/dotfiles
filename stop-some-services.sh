#!/bin/bash 

select SERVICE in $(sudo service --status-all \
		   |& grep "[+]" | awk '{print $4}'); do
    echo "stopping ${SERVICE} ..."
    sudo service ${SERVICE} stop
    echo "disable ${SERVICE} permanently?"
    select OPT in "Yes" "No"; do
	case $OPT in
	    Yes )
		echo "removing ${SERVICE} permanently"
		sudo update-rc.d ${SERVICE} remove
		break #uno
		;;
	esac
	break #dos
    done

done


