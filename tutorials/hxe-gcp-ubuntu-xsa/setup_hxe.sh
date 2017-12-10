#!/bin/bash

#
# This utility installs HANA, express edition.
#


# Check if this utility run as root.
checkRootUser() {
	if [ ${EUID} -ne 0 ]; then
		echo "You need to run \"${PROG_NAME}\" as root."
		exit 1
	fi
}

checkBash() {
	if [ -L /bin/sh ]; then
		local sh_link=`readlink -f /bin/sh`
		if ! echo $sh_link | grep -i "bash$" >& /dev/null; then
			echo "/bin/sh is a softlink to $sh_link.  HANA scripts are not compatible with $sh_link shell.  Run the following to change /bin/sh to Bash shell:" | fold -w 80 -s
			echo -e "\tsudo mv /bin/sh /bin/sh.orig"
			echo -e "\tsudo ln -s /bin/bash /bin/sh"
			exit 1
		fi
	fi
}

checkOpenSSL() {
	OPENSSL_VERSION=$(openssl version | cut -c9-14)
	OPENSSL_URL="https://www.openssl.org/source/openssl-${OPENSSL_VERSION}.tar.gz"
	openssl ciphers -ssl2 >& /dev/null
	if [ $? -eq 0 ]; then
		HAS_OPENSSL_V2=1
	fi
}

checkOSPackage() {
	for i in ${REQUIRE_OS_PACKAGE}; do
		if ! isOSPackageInstalled $i; then
			MISSING_OS_PACKAGE="${MISSING_OS_PACKAGE} $i"
		fi
	done
}

# Check if XSA is compatible with the installed HANA, express edition version
checkCompatibleXSAVersion() {
	hxe_version=$(getINIFileValue /usr/sap/${SID}/SYS/global/hdb/hxe_info.txt "hxe_version" "HANA, express edition")
	if [ "${BUILD_VERSION}" != "${hxe_version}" ]; then
		echo "Cannot install XSA.  This XSA version is not compatible with your installed HANA, express edition version."
		exit 1
	fi
}

promptOSPackage() {
	local tmp=
	if [ -n "${MISSING_OS_PACKAGE}" ]; then
		echo "HANA, express edition requires following missing package(s):"
		echo -e "\t${MISSING_OS_PACKAGE}"
		echo
		while [ 1 ]; do
			read -p "Would you like to install these packages (Y/N): " tmp
			if [ "$tmp" == "Y" -o "$tmp" == "y" ]; then
				break
			elif [ "$tmp" == "n" -o "$tmp" == "n" ]; then
				echo "Installation cannot be started without the required packages."
				exit 1
			else
				echo "Invalid input.  Enter \"Y\" or \"N\"."
			fi
		done
		echo
	fi
}

#
# Prompt to rebuilt OpenSSL with SSLv2/3 support
#
promptOpenSSL() {
	if [ $HAS_OPENSSL_V2 -ne 1 ]; then
		local tmp=
		echo "HANA, express edition installation requires OpenSSL library with SSLv2/SSLv3"
		echo "support.  Do you want to download OpenSSL soure code and rebuilt with"
		echo "SSLv2/SSLv3 support? This OpenSSL version will be deleted after installation."
		echo
		while [ 1 ]; do
			read -p "Download and rebuild OpenSSL from https://www.openssl.org? (Y/N): " tmp
			if [ "$tmp" == "Y" -o "$tmp" == "y" ]; then
				echo "Checking ${OPENSSL_URL}.  Please wait..."
				curl --output /dev/null --silent --head --fail "${OPENSSL_URL}"
				if [ $? -ne 0 ]; then
					echo
					echo "Cannot access ${OPENSSL_URL}."
					echo "Please check your network connection."
					echo
				fi
				break;
			elif [ "$tmp" == "n" -o "$tmp" == "n" ]; then
				echo "Installation cannot be started without OpenSSL library with SSLv2/SSLv3 support."
				exit 1
			else
				echo "Invalid input.  Enter \"Y\" or \"N\"."
			fi
		done
		echo
	fi
}

# Prompt HANA, express edition installer image root directory
promptImageRootDir() {
	# Check default location has valid image
	if [ ! -f "${IMAGE_DIR}/${DATA_UNITS_DIR}/${HDB_LCM_COMP_DIR}/hdblcm" ]; then
		IMAGE_DIR=""
	elif [ ! -f "${IMAGE_DIR}/hxe_optimize.sh" ]; then
		IMAGE_DIR=""
	elif [ -d "${IMAGE_DIR}/${DATA_UNITS_DIR}/${XSA_RT_COMP_DIR}" -a ! -f "${IMAGE_DIR}/register_cockpit.sh" ]; then
		IMAGE_DIR=""
	fi

	while [ 1 ]; do
		echo "Enter HANA, express edition installer root directory:"
		echo "    Hint: <extracted_path>/${HXE_DIR}"
		read -p "HANA, express edition installer root directory [${IMAGE_DIR}]: " tmp
		if [ -z "${tmp}" -a -n "${IMAGE_DIR}" ]; then
			break
		else
			if [ ! -d "${tmp}" -a ! -h "${tmp}" ]; then
				echo ""
				echo "\"${tmp}\" does not exist or not a directory."
				echo ""
			elif [ ! -f "${tmp}/${DATA_UNITS_DIR}/${HDB_LCM_COMP_DIR}/hdblcm" ]; then
				echo ""
				echo "\"${tmp}\" does not contain HANA, express edition installer."
				echo "Life cycle management utility \"${tmp}/DATA_UNITS/${HDB_LCM_COMP_DIR}/hdblcm\" does not exist."
				echo ""
			elif [ ! -f "${tmp}/hxe_optimize.sh" ]; then
				echo ""
				echo "Invalid HANA, express edition installer directory."
				echo "Cannot find hxe_optimize.sh utility in \"${tmp}\" directory."
				echo ""
			elif [ -d "${tmp}/${DATA_UNITS_DIR}/${XSA_RT_COMP_DIR}" -a ! -f "${tmp}/${DATA_UNITS_DIR}/${WEB_IDE_COMP_DIR}/${WEB_IDE_EXT_FILE}" ]; then
				echo
				echo "Invalid HANA, express edition installer directory."
				echo "Web IDE file \"${tmp}/${DATA_UNITS_DIR}/${WEB_IDE_COMP_DIR}/${WEB_IDE_EXT_FILE}\" does not exist."
				echo
			elif [ -d "${tmp}/${DATA_UNITS_DIR}/${XSA_RT_COMP_DIR}" -a ! -f "${tmp}/${DATA_UNITS_DIR}/${HANA_COCKPIT_COMP_DIR}/${HANA_COCKPIT_ZIP_FILE}" ]; then
				echo
				echo "Invalid HANA, express edition installer directory."
				echo "Cockpit file \"${tmp}/${DATA_UNITS_DIR}/${HANA_COCKPIT_COMP_DIR}/${HANA_COCKPIT_ZIP_FILE}\" does not exist."
				echo
			else
				IMAGE_DIR="${tmp}"
				break
			fi
		fi
	done
	if [ -d "${IMAGE_DIR}/${DATA_UNITS_DIR}/${HDB_SERVER_COMP_DIR}" ]; then
		IMAGE_HAS_SERVER=1
	fi
	
        if [ -d "${IMAGE_DIR}/${DATA_UNITS_DIR}/${XSA_RT_COMP_DIR}" ]; then
		IMAGE_HAS_XSA=1
	fi

	if [ -f "${IMAGE_DIR}/install_shine.sh" ]; then
		IMAGE_HAS_SHINE=1
	fi

	if [ -f "${IMAGE_DIR}/install_eadesigner.sh" ]; then
		IMAGE_HAS_EAD=1
        fi

	if [ -f "${IMAGE_DIR}/install_hsa.sh" ]; then
		IMAGE_HAS_SA=1
	fi

	if [ -f "${IMAGE_DIR}/install_sdi.sh" ]; then
		IMAGE_HAS_SDI=1
	fi

	if [ -d "${IMAGE_DIR}/${DATA_UNITS_DIR}/${DPA_DIR}" ]; then
		IMAGE_HAS_DPA=1
	fi

	if [ -f "${IMAGE_DIR}/install_eml.sh" ]; then
		IMAGE_HAS_EML=1
	fi
}

#Prompt for adding XSA
promptAddXSA(){
	echo "Detected server instance ${SID} without Extended Services + apps (XSA)"
	while [ 1 ] ; do
		read -p "Do you want to install XSA? (Y/N): " tmp
		if [ "$tmp" == "Y" -o "$tmp" == "y" ]; then
			ADD_XSA=1
			COMPONENT="all"
			break
		elif [ "$tmp" == "N" -o "$tmp" == "n" ]; then
			ADD_XSA=0
			return
		else
			echo "Invalid input.  Enter \"Y\" or \"N\"."
		fi
	done

	echo
}

# Prompt which component to install
promptComponent() {
	if [ ${IMAGE_HAS_XSA} -eq 1 ]; then
		# default to "all" if we have XSA in image
		COMPONENT="all"
		while [ 1 ]; do
			echo "Enter component to install:"
			echo "   server - HANA server + Application Function Library"
			echo "   all    - HANA server, Application Function Library, Extended Services + apps (XSA)"
			read -p "Component [${COMPONENT}]: " comp
			if [ -z "${comp}" -a -n "${COMPONENT}" ]; then
				break
			elif [ "${comp}" != "server" -a "${comp}" != "all" ]; then
				echo ""
				echo "\"${comp}\" is invalid component."
				echo ""
			else
				COMPONENT="${comp}"
				break
			fi
		done
	else
		# No XSA - default to "server"
		COMPONENT="server"
	fi

	echo
}
#
# Prompt local host name
#
promptHostName() {
	while [ 1 ]; do
		read -p "Enter local host name [${HOST_NAME}]: " host
		if [ -z "${host}" -a -n "${HOST_NAME}" ]; then
			break
		elif [ -z "${host}" -a -z "${HOST_NAME}" ]; then
			echo ""
			echo "Please enter local host."
			echo ""
		else
			HOST_NAME="${host}"
			break
		fi
	done

	echo
}
#
# Prompt HANA system ID
#
promptSID() {
	while [ 1 ]; do
		read -p "Enter SAP HANA system ID [${SID}]: " id
		if [ -z "${id}" -a -n "${SID}" ]; then
			break
		else
			if [[ ${id} =~ ^[A-Z][A-Z,0-9][A-Z,0-9]$ ]]; then
				SID="${id}"
				break
			else
				echo ""
				echo "Invalid SAP HANA system ID.  This must be a three characters string."
				echo "First character has to be an upper case letter."
				echo "Second and third characters can be upper case letter or a decimal digit."
				echo ""
			fi
                fi
        done

	HXE_ADM_USER=`echo ${SID}adm | tr '[:upper:]' '[:lower:]'`

	echo
}
#
# Search for existing SID
#
searchSID() {
	if [ -f /hana/shared/${SID}/global/hdb/nameserver.lck ]; then
		HAS_SERVER=1
	fi
	if [ -f /hana/shared/${SID}/xs/bin/hdbxscontroller ]; then
		HAS_XSA=1
	fi
}

#
# Prompt HANA instance number
#
promptInstance() {
	while [ 1 ]; do
		read -p "Enter HANA instance number [${INSTANCE}]: " num
		if [ "${num}" == "" ]; then
			break
		elif ! [[ ${num} =~ ^[0-9]+$ ]] ; then
			echo ""
			echo "\"$num\" is not a number.  Enter a number between 00 and 99."
			echo ""
		elif [ ${num} -ge 0 -a ${num} -le 99 ]; then
			if [[ ${num} =~ ^[0-9]$ ]] ; then
				num="0${num}"
			fi
			INSTANCE="${num}"
			break
		else
			echo ""
			echo "Invalid number.  Enter a number between 00 and 99."
			echo ""
		fi
	done

	echo
}

# Prompt user password
# arg 1: user name
# arg 2: variable name to store password value
#
promptPwd() {
	local pwd=""
	read -r -s -p "Enter \"${1}\" password : " pwd
	echo
	eval $2=\$pwd
}

#
# Prompt new user password
# arg 1: user name
# arg 2: variable name to store password value
#
promptNewPwd() {
	local pwd=""
	local confirm_pwd=""

	echo
	echo "Password must be at least 8 characters in length.  It must contain at least"
	echo "1 uppercase letter, 1 lowercase letter, and 1 number.  Special characters"
	echo "are allowed, except \\ (backslash), ' (single quote), \" (double quotes),"
	echo "\` (backtick), and \$ (dollar sign)."
	echo
	while [ 1 ] ; do
		read -r -s -p "Enter ${1} password: " pwd
		echo

		if ! isValidPassword $pwd; then
			continue
		fi

		read -r -s -p "Confirm \"${1}\" password: " confirm_pwd
		echo
		if [ "${pwd}" != "${confirm_pwd}" ]; then
			echo ""
			echo "Passwords do not match."
			echo ""
			continue
		fi

		eval $2=\$pwd

		break;
	done

	echo
}

#
# Prompt proxy host and port
#
promptProxyInfo() {
	getSystemHTTPProxy

	while [ 1 ] ; do
		read -p "Do you need to use proxy server to access the internet? (Y/N): " tmp
		if [ "$tmp" == "Y" -o "$tmp" == "y" ]; then
			SETUP_PROXY=1
			break
		elif [ "$tmp" == "N" -o "$tmp" == "n" ]; then
			SETUP_PROXY=0
			echo
			return
		else
			echo "Invalid input.  Enter \"Y\" or \"N\"."
		fi
	done

	# Proxy host
	while [ 1 ]; do
		read -p "Enter proxy host name [$SYSTEM_PROXY_HOST]: " tmp
		if [ -z "$tmp" ]; then
			if [ -n "$SYSTEM_PROXY_HOST" ]; then
				tmp="$SYSTEM_PROXY_HOST"
			else
				continue
			fi
		fi
		if ! $(isValidHostName "$tmp"); then
			echo
			echo "\"$tmp\" is not a valid host name or IP address."
			echo
		else
			PROXY_HOST="$tmp"
			break
		fi
	done

	# Proxy port
	while [ 1 ]; do
		read -p "Enter proxy port number [$SYSTEM_PROXY_PORT]: " tmp
		if [ -z "$tmp" ]; then
			if [ -n "$SYSTEM_PROXY_PORT" ]; then
				tmp="$SYSTEM_PROXY_PORT"
			else
				continue
			fi
		fi
		if ! $(isValidPort "$tmp"); then
			echo
			echo "\"$tmp\" is not a valid port number."
			echo "Enter number between 1 and 65535."
			echo
		else
			PROXY_PORT="$tmp"
			break
		fi
	done

	# No proxy hosts
	read -p "Enter comma separated domains that do not need proxy [$SYSTEM_NO_PROXY_HOST]: " tmp
	if [ -z "$tmp" ]; then
		NO_PROXY_HOST="$SYSTEM_NO_PROXY_HOST"
	else
		NO_PROXY_HOST="$tmp"
	fi

	echo
}

#
# Prompt to install SHINE
#
promptInstallShine() {
	echo "SAP HANA Interactive Education, or SHINE, is a demo application that makes"
	echo "it easy to learn how to build applications on SAP HANA Extended Application"
	echo "Services Advanced Model."
	while [ 1 ] ; do
		read -p "Install SHINE? (Y/N) : " proceed
		if [ "${proceed}" == "Y" -o "${proceed}" == "y" ]; then
			INSTALL_SHINE=1
			break
		elif [ "${proceed}" == "N" -o "${proceed}" == "n" ]; then
			INSTALL_SHINE=0
			break
		fi
	done

	echo
}

#
# Prompt to install EA Designer
#
promptInstallEADesigner() {
	echo "SAP EA Designer lets you capture, analyze, and present your organization's"
	echo "landscapes, strategies, requirements, processes, data, and other artifacts"
	echo "in a shared environment."
	while [ 1 ] ; do
		read -p "Install SAP EA Designer? (Y/N) : " proceed
		if [ "${proceed}" == "Y" -o "${proceed}" == "y" ]; then
			INSTALL_EAD=1
			break
		elif [ "${proceed}" == "N" -o "${proceed}" == "n" ]; then
			INSTALL_EAD=0
			break
		fi
	done

	echo
}

#
# Prompt to install SA
#
promptInstallSA() {
	echo "The SAP HANA streaming analytics option processes high-velocity, high-volume"
	echo "event streams in real time, allowing you to filter, aggregate, and enrich raw"
	echo "data before committing it to your database."
	while [ 1 ] ; do
		read -p "Install SAP HANA streaming analytics? (Y/N) : " proceed
		if [ "${proceed}" == "Y" -o "${proceed}" == "y" ]; then
			INSTALL_SA=1
			break
		elif [ "${proceed}" == "N" -o "${proceed}" == "n" ]; then
			INSTALL_SA=0
			break
		fi
	done

	echo

	if [ $INSTALL_SA -eq 1 ]; then
		local db_name=""
		if [ -z "$SA_TENANT_DB" ]; then
			SA_TENANT_DB="$SID"
		fi
		while [ 1 ]; do
			read -p "Enter new tenant database to add streaming [${SA_TENANT_DB}]: " db_name
			if [ -z "$db_name" ]; then
				db_name="$SA_TENANT_DB"
			fi
			local tmp=`echo $db_name | tr '[:upper:]' '[:lower:]'`
			if [ "$db_name" == "systemdb" ]; then
				echo
				echo "SystemDB database not supported. Only applicable to user databases."
				echo
			else
				SA_TENANT_DB="$db_name"
				break;
			fi
		done
	fi

	echo
}

#
# Prompt to install SDI
#
promptInstallSDI() {
	echo "SAP HANA smart data integration provides functionality to access source data, and provision, replicate, and transform that data in SAP HANA on-premise or in the cloud." | fold -w 80 -s

	while [ 1 ] ; do
		read -p "Install SAP HANA smart data integration? (Y/N) : " proceed
		if [ "${proceed}" == "Y" -o "${proceed}" == "y" ]; then
			INSTALL_SDI=1
			break
		elif [ "${proceed}" == "N" -o "${proceed}" == "n" ]; then
			INSTALL_SDI=0
			break
		fi
	done

	echo

	if [ $INSTALL_SDI -eq 1 ]; then
		local db_name=""
		if [ -z "$SDI_TENANT_DB" ]; then
			SDI_TENANT_DB="$SID"
		fi
		while [ 1 ]; do
			read -p "Enter name of tenant database to add smart data integration [${SDI_TENANT_DB}]: " db_name
			if [ -z "$db_name" ]; then
				db_name="$SDI_TENANT_DB"
			fi
			local tmp=`echo $db_name | tr '[:upper:]' '[:lower:]'`
			if [ "$tmp" == "systemdb" ]; then
				echo
				echo "SystemDB database not supported. Only applicable to user databases."
				echo
			else
				SDI_TENANT_DB="$db_name"
				break;
			fi
		done
	fi

	echo
}

#
# Prompt to install DP Agent
#
promptInstallDPA() {
	echo "The Data Provisioning Agent (DP Agent) provides secure connectivity between the SAP HANA database and your on-premise, adapter-based sources." | fold -w 80 -s

	while [ 1 ] ; do
		read -p "Install DP Agent? (Y/N) : " proceed
		if [ "${proceed}" == "Y" -o "${proceed}" == "y" ]; then
			INSTALL_DPA=1
			break
		elif [ "${proceed}" == "N" -o "${proceed}" == "n" ]; then
			INSTALL_DPA=0
			break
		fi
	done

	echo

	if [ $INSTALL_DPA -eq 1 ]; then
		#  Agent installation path
		local install_path=""
		read -p "Enter DP Agent installation path [${DPA_INSTALL_PATH}]: " install_path
		if [ -n "$install_path" ]; then
			DPA_INSTALL_PATH="$install_path"
		fi
		echo

		# User name for Agent service
		local agent_user=""
		if [ -z "$DPA_USER" ]; then
			DPA_USER=$(/usr/bin/logname)
			if [ $? -ne 0 ]; then
				DPA_USER=""
			fi
		fi
		while [ 1 ]; do
			read -p "Enter User name for Agent service (user must exist) [${DPA_USER}]: " agent_user
			if [ -z "$agent_user" -a -n "$DPA_USER" ]; then
				agent_user="$DPA_USER"
			fi

			if [ -n "$agent_user" ]; then
				if ! $(/usr/bin/id -u $agent_user >& /dev/null); then
					echo
					echo "User \"$agent_user\" does not exist."
					echo
				else
					DPA_USER=$agent_user
					break
				fi
			fi
		done
		echo

		# agent listener port
		local port=""
		if [ -z "$DPA_LISTENER_PORT" ]; then
			DPA_LISTENER_PORT=5051
		fi
		while ! $(isFreePort "$DPA_LISTENER_PORT"); do
			(( DPA_LISTENER_PORT++ ))
		done
		while [ 1 ]; do
			read -p "Enter agent listener port [${DPA_LISTENER_PORT}]: " port
			if [ -z "$port" ]; then
				port=$DPA_LISTENER_PORT
			fi

			if ! $(isValidPort "$port"); then
				echo
				echo "\"$port\" is not a valid port number."
				echo "Enter number between 1 and 65535."
				echo
			elif ! $(isFreePort "$port"); then
				echo
				echo "Port \"$port\" is already used.  Enter different port."
				echo
			else
				DPA_LISTENER_PORT=$port
				break
			fi
		done
		echo

		# agent administration port
		if [ -z "$DPA_ADMIN_PORT" ]; then
			DPA_ADMIN_PORT=5050
		fi
		while ! $(isFreePort "$DPA_ADMIN_PORT"); do
			(( DPA_ADMIN_PORT++ ))
		done
		while [ 1 ]; do
			read -p "Enter agent administration port [${DPA_ADMIN_PORT}]: " port
			if [ -z "$port" ]; then
				port=$DPA_ADMIN_PORT
			fi

			if ! $(isValidPort "$port"); then
				echo
				echo "\"$port\" is not a valid port number."
				echo "Enter number between 1 and 65535."
				echo
			elif [ "$port" == "DPA_ADMIN_PORT" ]; then
				echo
				echo "Port \"$port\" is already specified for agent listener.  Enter different port."
				echo
			elif ! $(isFreePort "$DPA_ADMIN_PORT"); then
				echo
				echo "Port \"$port\" is already used.  Enter different port."
				echo
			else
				DPA_ADMIN_PORT=$port
				break
			fi
		done
		echo
	fi
}

#
# Prompt to install SAP HANA External Machine Learning Library
promptInstallEML() {
	echo "The SAP HANA External Machine Learning Library is an application function library (AFL) supporting the integration of Google TensorFlow, as an external machine learning framework, with SAP HANA, express edition." | fold -w 80 -s
	while [ 1 ] ; do
		read -p "Install SAP HANA External Machine Learning Library? (Y/N) : " proceed
		if [ "${proceed}" == "Y" -o "${proceed}" == "y" ]; then
			INSTALL_EML=1
			break
		elif [ "${proceed}" == "N" -o "${proceed}" == "n" ]; then
			INSTALL_EML=0
			break
		fi
	done

	echo
}

# Check if tenant database exits
# $1 - database
hasDatabase() {
	execSQL ${INSTANCE} SystemDB SYSTEM ${MASTER_PWD} "SELECT COUNT(*) from \"PUBLIC\".\"M_DATABASES\" WHERE DATABASE_NAME='${1}'"
	SQL_OUTPUT=`trim ${SQL_OUTPUT}`
	if [ "${SQL_OUTPUT}" == "1" ]; then
		return 0
	fi

	return 1
}

# Check if tenant DB started
# $1 - database
isTenantDbStarted() {
	local output=$(su -l ${HXE_ADM_USER} -c "/usr/sap/${SID}/HDB${INSTANCE}/exe/hdbsql -a -x -quiet 2>&1 <<-EOF
\c -i ${INSTANCE} -d $1 -u foo -p no_password
EOF
"
)
	if echo "$output" | grep -i "authentication failed" >& /dev/null; then
		return 0
	fi

	return 1
}

# Start tenant DB
# $1 - database
startTenantDB() {
	local db_name="$1"
	if ! $(isTenantDbStarted ${db_name}); then
		echo "Start \"${db_name}\" tenant database..."
		execSQL ${INSTANCE} SystemDB SYSTEM ${MASTER_PWD} "ALTER SYSTEM START DATABASE ${db_name}"
	fi
}

# Stop tenant DB
# $1 - database
stopTenantDB() {
	local db_name="$1"
	if $(isTenantDbStarted ${db_name}); then
		echo "Stop \"${db_name}\" tenant database..."
		execSQL ${INSTANCE} SystemDB SYSTEM ${MASTER_PWD} "ALTER SYSTEM STOP DATABASE ${db_name}"
	fi
}

#
# Execute SQL statement and store output to SQL_OUTPUT
# $1 - instance #
# $2 - database
# $3 - user
# $4 - password
# $5 - SQL
execSQL() {
	local db="$2"
	local db_lc=`echo "$2" | tr '[:upper:]' '[:lower:]'`
	if [ "${db_lc}" == "systemdb" ]; then
		db="SystemDB"
	fi
	local sql="$5"
	SQL_OUTPUT=$(su -l ${HXE_ADM_USER} -c "/usr/sap/${SID}/HDB${1}/exe/hdbsql -a -x -quiet 2>&1 <<-EOF
\c -i $1 -d $db -u $3 -p $4
$sql
EOF
"
)
	if [ $? -ne 0 ]; then
		# Strip out password string
		if [ -n "${4}" ]; then
			sql=`echo "${sql}" | sed "s/${4}/********/g"`
		fi
		if [ -n "${MASTER_PWD}" ]; then
			sql=`echo "${sql}" | sed "s/${MASTER_PWD}/********/g"`
		fi
		echo "hdbsql $db => ${sql}"
		echo "${SQL_OUTPUT}"
		return 1
	fi
}

#
formatNoProxyHost()
{
        if [ -z "$1" ]; then
                return
        fi

        local no_ph=""
        IFS=',' read -ra hlist <<< "$1"
        for i in "${hlist[@]}"; do
                tmp=$(trim "$i")
                if [ -n "$tmp" ]; then
                        if [[ "$tmp" =~ ^[0-9]*\. ]] || [[ "$tmp" =~ [Ll][Oo][Cc][Aa][Ll][Hh][Oo][Ss][Tt] ]]; then
                                no_ph="$no_ph,$tmp"
                        elif [[ "$tmp" =~ ^\. ]]; then
                                no_ph="$no_ph,*$tmp"
                        else
                                no_ph="$no_ph,*.$tmp"
                        fi
                fi
        done
	unset IFS
        echo $no_ph | sed 's/^,//'
}

#
# Get the system proxy host and port
#
getSystemHTTPProxy() {
	local url="$https_proxy"
	local is_https_port=1

	if [ -z "$url" ]; then
		url="$http_proxy"
		is_https_port=0
	fi
	if [ -z "$url" ] && [ -f /etc/sysconfig/proxy ]; then
		url=`grep ^HTTPS_PROXY /etc/sysconfig/proxy | cut -d'=' -f2`
		is_https_port=1
	fi
	if [ -z "$url" ] && [ -f /etc/sysconfig/proxy ]; then
		url=`grep ^HTTP_PROXY /etc/sysconfig/proxy | cut -d'=' -f2`
		is_https_port=0
	fi

	url="${url%\"}"
	url="${url#\"}"
	url="${url%\'}"
        url="${url#\'}"

	if [ -z "$url" ]; then
		SETUP_PROXY=0
		return
	fi

	# Get proxy host
	SYSTEM_PROXY_HOST=$url
	if echo $url | grep -i '^http' >& /dev/null; then
		SYSTEM_PROXY_HOST=`echo $url | cut -d '/' -f3 | cut -d':' -f1`
	else
		SYSTEM_PROXY_HOST=`echo $url | cut -d '/' -f1 | cut -d':' -f1`
	fi

	if [ -n "${SYSTEM_PROXY_HOST}" ]; then
		SETUP_PROXY=1
	fi

	# Get proxy port
	if echo $url | grep -i '^http' >& /dev/null; then
		if echo $url | cut -d '/' -f3 | grep ':' >& /dev/null; then
			SYSTEM_PROXY_PORT=`echo $url | cut -d '/' -f3 | cut -d':' -f2`
		elif [ $is_https_port -eq 1 ]; then
			SYSTEM_PROXY_PORT="443"
		else
			SYSTEM_PROXY_PORT="80"
		fi
	else
		if echo $url | cut -d '/' -f1 | grep ':' >& /dev/null; then
			SYSTEM_PROXY_PORT=`echo $url | cut -d '/' -f1 | cut -d':' -f2`
		elif [ $is_https_port -eq 1 ]; then
			SYSTEM_PROXY_PORT="443"
		else
			SYSTEM_PROXY_PORT="80"
		fi
        fi

	# Get no proxy hosts
	SYSTEM_NO_PROXY_HOST="$no_proxy"
	if [ -z "$SYSTEM_NO_PROXY_HOST" ] && [ -f /etc/sysconfig/proxy ]; then
		SYSTEM_NO_PROXY_HOST=`grep ^NO_PROXY /etc/sysconfig/proxy | cut -d'=' -f2`
		SYSTEM_NO_PROXY_HOST="${SYSTEM_NO_PROXY_HOST%\"}"
		SYSTEM_NO_PROXY_HOST="${SYSTEM_NO_PROXY_HOST#\"}"
		SYSTEM_NO_PROXY_HOST="${SYSTEM_NO_PROXY_HOST%\'}"
		SYSTEM_NO_PROXY_HOST="${SYSTEM_NO_PROXY_HOST#\'}"
	fi
	if [ -z "$SYSTEM_NO_PROXY_HOST" ] && [ -f /etc/sysconfig/proxy ]; then
		SYSTEM_NO_PROXY_HOST=`grep ^no_proxy /etc/sysconfig/proxy | cut -d'=' -f2`
		SYSTEM_NO_PROXY_HOST="${SYSTEM_NO_PROXY_HOST%\"}"
		SYSTEM_NO_PROXY_HOST="${SYSTEM_NO_PROXY_HOST#\"}"
		SYSTEM_NO_PROXY_HOST="${SYSTEM_NO_PROXY_HOST%\'}"
		SYSTEM_NO_PROXY_HOST="${SYSTEM_NO_PROXY_HOST#\'}"
	fi
        if [ -n "$SYSTEM_NO_PROXY_HOST" ]; then
                SYSTEM_NO_PROXY_HOST=$(formatNoProxyHost "$SYSTEM_NO_PROXY_HOST")
        fi

}

isValidHostName() {
	local hostname_regex='^(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]*[a-zA-Z0-9])\.)*([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9\-]*[A-Za-z0-9])$'
	echo "$1" | egrep $hostname_regex >& /dev/null
}

isValidPort() {
	if [[ $1 =~ ^[0-9]?+$ ]]; then
		if [ $1 -ge 1 ] && [ $1 -le 65535 ]; then
			return 0
		else
			return 1
		fi
	else
		return 1
	fi
}

isFreePort() {
	if ! $(/bin/netstat -tulpn | grep :${1} >& /dev/null); then
		return 0
	fi

	return 1
}

isValidPassword() {
	local msg=""
	local showPolicy=0
	local pwd="$1"
	local crack_msg=""

	if [ `echo "$pwd" | wc -c` -le 8 ]; then
		msg="too short"
		showPolicy=1
	fi
	if ! echo "$pwd" | grep "[A-Z]" >& /dev/null; then
		if [ -z "$msg" ]; then
			msg="missing uppercase letter"
		else
			msg="$msg, missing uppercase letter"
		fi
		showPolicy=1
	fi
	if ! echo "$pwd" | grep "[a-z]" >& /dev/null; then
		if [ -z "$msg" ]; then
			msg="missing lowercase letter"
		else
			msg="$msg, missing lowercase letter"
		fi
		showPolicy=1
	fi
	if ! echo "$pwd" | grep "[0-9]" >& /dev/null; then
		if [ -z "$msg" ]; then
			msg="missing a number"
		else
			msg="$msg, missing a number"
		fi
		showPolicy=1
	fi
	if echo "$pwd" | grep -F '\' >& /dev/null; then
		if [ -z "$msg" ]; then
			msg="\\ (backslash) not allowed"
		else
			msg="$msg, \\ (backslash) not allowed"
		fi
		showPolicy=1
	fi
	if echo "$pwd" | grep -F "'" >& /dev/null; then
		if [ -z "$msg" ]; then
			msg="' (single quote) not allowed"
		else
			msg="$msg, ' (single quote) not allowed"
		fi
		showPolicy=1
	fi
	if echo "$pwd" | grep -F '"' >& /dev/null; then
		if [ -z "$msg" ]; then
			msg="\" (double quotes) not allowed"
		else
			msg="$msg, \" (double quotes) not allowed"
		fi
		showPolicy=1
	fi
	if echo "$pwd" | grep -F '`' >& /dev/null; then
		if [ -z "$msg" ]; then
			msg="\` (backtick) not allowed"
		else
			msg="$msg, \` (backtick) not allowed"
		fi
		showPolicy=1
	fi
	if echo "$pwd" | grep -F '$' >& /dev/null; then
		if [ -z "$msg" ]; then
			msg="\$ (dollar sign) not allowed"
		else
			msg="$msg, \$ (dollar sign) not allowed"
		fi
		showPolicy=1
	fi
	if [ $showPolicy -eq 1 ]; then
		echo
		echo "Invalid password: ${msg}." | fold -w 80 -s
		echo
		echo "Password must meet all of the following criteria:"
		echo "- 8 or more letters"
		echo "- At least 1 uppercase letter"
		echo "- At least 1 lowercase letter"
		echo "- At least 1 number"
		echo
		echo "Special characters are optional; except \\ (backslash), ' (single quote),"
		echo "\" (double quotes), \` (backtick), and \$ (dollar sign)."
		echo
		return 1
	fi

	crack_msg=`/usr/sbin/cracklib-check <<EOF
${pwd}
EOF`
	crack_msg=`awk 'BEGIN { FS = ": " } ; {print $2}' <<EOF
${crack_msg}
EOF`
	if [ "${crack_msg}" != "OK" ] ; then
		echo
		echo "Invalid password: ${crack_msg}"
		echo
		return 1
	fi

	return 0
}

#
# Print pre-install summary
#
printSummary() {
	comp_desc="HANA server + Application Function Library"
	if [ ${ADD_XSA} -eq 1 ]; then
		comp_desc="Extended Services + apps (XSA)"
	elif [ "${COMPONENT}" == "all" ]; then
		comp_desc="HANA server, Application Function Library, and Extended Services + apps (XSA)"
	fi
	echo ""
	echo "##############################################################################"
	echo "# Summary before execution                                                   #"
	echo "##############################################################################"
	echo "HANA, express edition installer : ${IMAGE_DIR}"
	echo "  Component(s) to install                   : ${comp_desc}"
	echo "  Host name                                 : ${HOST_NAME}"
	echo "  HANA system ID                            : ${SID}"
	echo "  HANA instance number                      : ${INSTANCE}"
	echo "  Master password                           : ********"
	echo "  Log file                                  : ${LOG_FILE}"
	if [ "${COMPONENT}" == "all" ]; then
		if [ $DO_NOT_REGISTER_RESOURCE -eq 1 ]; then
			echo "  Do not register cockpit resources         : Yes"
		fi

		if [ $SETUP_PROXY -eq 1 ]; then
			echo "  Proxy host                                : ${PROXY_HOST}"
			echo "  Proxy port                                : ${PROXY_PORT}"
			echo "  Hosts with no proxy                       : ${NO_PROXY_HOST}"
		else
			echo "  Proxy host                                : N/A"
			echo "  Proxy port                                : N/A"
			echo "  Hosts with no proxy                       : N/A"
		fi

		if [ ${IMAGE_HAS_SHINE} -eq 1 ];then
			if [ $INSTALL_SHINE -eq 1 ]; then
				echo "  Install SHINE                             : Yes"
			else
				echo "  Install SHINE                             : No"
			fi
		fi

		if [ ${IMAGE_HAS_EAD} -eq 1 ];then
			if [ $INSTALL_EAD -eq 1 ]; then
				echo "  Install SAP EA Designer                   : Yes"
			else
				echo "  Install SAP EA Designer                   : No"
			fi
		fi
	fi

	if [ ${IMAGE_HAS_SA} -eq 1 ]; then
		if [ $INSTALL_SA -eq 1 ]; then
			echo "  Install Streaming Analytics               : Yes"
			echo "  Tenant database to add streaming          : ${SA_TENANT_DB}"
		else
			echo "  Install Streaming Analytics               : No"
		fi
	fi
	if [ ${IMAGE_HAS_SDI} -eq 1 ]; then
		if [ $INSTALL_SDI -eq 1 ]; then
			echo "  Install SDI                               : Yes"
			echo "  Tenant database to add SDI                : ${SDI_TENANT_DB}"
		else
			echo "  Install SDI                               : No"
		fi
	fi
	if [ ${IMAGE_HAS_DPA} -eq 1 ]; then
		if [ $INSTALL_DPA -eq 1 ]; then
			echo "  Install DP Agent                          : Yes"
			echo "  DP Agent installation path                : ${DPA_INSTALL_PATH}"
			echo "  User name for Agent service               : ${DPA_USER}"
			echo "  DP Agent listener port                    : ${DPA_LISTENER_PORT}"
			echo "  DP Agent administration port              : ${DPA_ADMIN_PORT}"
		else
			echo "  Install DP Agent                          : No"
		fi
	fi
	if [ ${IMAGE_HAS_EML} -eq 1 ];then
		if [ $INSTALL_EML -eq 1 ]; then
			echo "  Install External Machine Learning Library : Yes"
		else
			echo "  Install External Machine Learning Library : No"
		fi
	fi
	if [ ${#HDBLCM_ARGS[@]} -ne 0 ]; then
		echo "  HDBLCM paramaters                         : ${HDBLCM_ARGS[@]}"
	fi

	echo
	if [ $BATCH_MODE -eq 0 ]; then
		while [ 1 ] ; do
			read -p "Proceed with installation? (Y/N) : " proceed
			if [ "${proceed}" == "Y" -o "${proceed}" == "y" ]; then
				echo
				return
			elif [ "${proceed}" == "N" -o "${proceed}" == "n" ]; then
				exit 1
			fi
		done
	fi
}

# Install missing OS packages
installOSPackage() {
	for i in ${MISSING_OS_PACKAGE}; do
		echo "Install package $i..."
		if [[ ${OS_VENDOR} =~ [Ss][Ll][Ee][Ss] ]]; then
			zypper -n install $i
			if [ $? -ne 0 ]; then
				echo "Failed to install packages $i."
				exit 1
			fi
		elif [[ ${OS_VENDOR} =~ [Uu][Bb][Uu][Nn][Tt][Uu] ]]; then
			apt-get -qq -y install $i
			if [ $? -ne 0 ]; then
				echo "Failed to install packages $i."
				exit 1
			fi
		elif [[ ${OS_VENDOR} =~ [Rr][Ee][Dd][Hh][Aa][Tt] ]]; then
			rpm --install $i
			if [ $? -ne 0 ]; then
				echo "Failed to install packages $i."
				exit 1
			fi
		fi
		echo
	done
	echo
}

#
# Download and build OpenSSL
downloadInstallOpenSSL() {
	if [ $HAS_OPENSSL_V2 -eq 1 ]; then
		return
	fi

	local save_workdir=`pwd`
	local output=

	echo "Download and build OpenSSL ${OPENSSL_VERSION}..."
	mkdir ${OPENSSL_TMP}
	chmod 755 ${OPENSSL_TMP}
	cd ${OPENSSL_TMP}
	curl -o ${OPENSSL_TMP}/openssl-${OPENSSL_VERSION}.tar.gz ${OPENSSL_URL}
	if [ $? -ne 0 ]; then
		echo "Failed to download ${OPENSSL_URL}"
		exit 1
	fi

	echo "Extracting openssl-${OPENSSL_VERSION}.tar.gz..."
	tar xzf ${OPENSSL_TMP}/openssl-${OPENSSL_VERSION}.tar.gz
	cd openssl-${OPENSSL_VERSION}

	echo "Building OpenSSL ${OPENSSL_VERSION}..."
	output=`./config --prefix=${OPENSSL_TMP}/openssl-${OPENSSL_VERSION}/local enable-ssl2 enable-ssl3 enable-shared`
	if [ $? -ne 0 ]; then
		echo $output
		echo "Failed to build OpenSSL ${OPENSSL_VERSION}."
		exit 1
	fi
	output=`make -s depend`
	if [ $? -ne 0 ]; then
		echo $output
		echo "Failed to build OpenSSL ${OPENSSL_VERSION}."
		exit 1
	fi
	output=`make -i -s build_libs build_apps install_sw`
	if [ $? -ne 0 ]; then
		echo $output
		echo "Failed to build OpenSSL ${OPENSSL_VERSION}."
		exit 1
	fi

	export PATH=${OPENSSL_TMP}/openssl-${OPENSSL_VERSION}/local/bin:${PATH}
	export LD_LIBRARY_PATH=${OPENSSL_TMP}/openssl-${OPENSSL_VERSION}/local/lib:${LD_LIBRARY_PATH}
	echo "${OPENSSL_TMP}/openssl-${OPENSSL_VERSION}/local/lib" >> ${OPENSSL_LD_CONF}
	ldconfig

	cd $save_workdir
}

#
# Install server component
#
installServer() {
         if [ ${ADD_XSA} -ne 1 ]; then
		local status=0
		echo "Installing HDB server..."
		local log_begin=`wc -l $LOG_FILE | cut -d' ' -f1`
		"${IMAGE_DIR}/${DATA_UNITS_DIR}/${HDB_LCM_COMP_DIR}/hdblcm" -s ${SID} -n ${INSTANCE} -H ${HOST_NAME} --components="server,afl" --hdbinst_server_import_content=off --read_password_from_stdin=xml ${HDBLCM_ARGS[@]} -b <<-EOF
<?xml version="1.0" encoding="UTF-8"?><Passwords><password><![CDATA[${MASTER_PWD}]]></password><system_user_password><![CDATA[${MASTER_PWD}]]></system_user_password><sapadm_password><![CDATA[${MASTER_PWD}]]></sapadm_password><xs_master_password><![CDATA[${MASTER_PWD}]]></xs_master_password><org_manager_password><![CDATA[${MASTER_PWD}]]></org_manager_password><master_password><![CDATA[${MASTER_PWD}]]></master_password></Passwords>
EOF
		status=$?
		local log_end=`wc -l $LOG_FILE | cut -d' ' -f1`
		if [ $status -ne 0 ] || sed -n "${log_begin},${log_end}p" $LOG_FILE | grep -i "fail\|error\|cannot find\|No such file\|is invalid" >& /dev/null; then
		echo
			echo "Failed to install HDB server."
			postInstallSummary 1 "Server Installation"
			appendRemainTasksStatusToPostSummary
			exit 1
		else
			postInstallSummary 0 "Server Installation"
		fi

		# Sleep to allow OS to catch up changes and server started
		sleep 180s

		chmod 755 /usr/sap/${SID}/home/.profile
		sed -i "s|^if \[ -f \$HOME/\.bashrc.*|if \[ -f \$HOME/\.bashrc -a -z \"\$SAPSYSTEMNAME\" \]; then|" /usr/sap/${SID}/home/.profile

		echo "Enable AFL..."
		execSQL ${INSTANCE} SystemDB SYSTEM ${MASTER_PWD} "alter system alter configuration ('indexserver.ini','SYSTEM') SET ('calcengine','llvm_lib_whitelist') = 'bfl,pal'"

		# Copy change_key.sh to <hxeadm home>/bin directory
		echo "mkdir -p /usr/sap/${SID}/home/bin" | su -l ${HXE_ADM_USER}
		echo "cp -p ${IMAGE_DIR}/change_key.sh /usr/sap/${SID}/home/bin" | su -l ${HXE_ADM_USER}
		echo "chmod 755 /usr/sap/${SID}/home/bin/change_key.sh" | su -l ${HXE_ADM_USER}

		# Copy hxe_gc.sh to <hxeadm home>/bin directory
		echo "mkdir -p /usr/sap/${SID}/home/bin" | su -l ${HXE_ADM_USER}
		echo "cp -p ${IMAGE_DIR}/hxe_gc.sh /usr/sap/${SID}/home/bin" | su -l ${HXE_ADM_USER}
		echo "chmod 755 /usr/sap/${SID}/home/bin/hxe_gc.sh" | su -l ${HXE_ADM_USER}
                
               # Update Versions
               updateVersionFile

		collectGarbage
	fi
}

#
# Install XSC
#
installXSC() {
	if [ ${ADD_XSA} -ne 1 ]; then
		echo "Installing XSC..."
		echo "Importing delivery units..."
		"/hana/shared/${SID}/global/hdb/install/bin/hdbupdrep" --content_directory="/hana/shared/${SID}/global/hdb/auto_content" --read_password_from_stdin=xml -b <<-EOF
<?xml version="1.0" encoding="UTF-8"?><Passwords><password><![CDATA[${MASTER_PWD}]]></password><system_user_password><![CDATA[${MASTER_PWD}]]></system_user_password><sapadm_password><![CDATA[${MASTER_PWD}]]></sapadm_password><xs_master_password><![CDATA[${MASTER_PWD}]]></xs_master_password><org_manager_password><![CDATA[${MASTER_PWD}]]></org_manager_password><master_password><![CDATA[${MASTER_PWD}]]></master_password></Passwords>
EOF
		if [ $? -ne 0 ]; then
		echo
			echo "Failed to import delivery units."
			postInstallSummary 1 "XSC Installation"
			appendRemainTasksStatusToPostSummary
			exit 1
		fi

		"/hana/shared/${SID}/global/hdb/install/bin/hdbupdrep" --content_directory="/hana/shared/${SID}/global/hdb/auto_content/systemdb" --read_password_from_stdin=xml -b <<-EOF
<?xml version="1.0" encoding="UTF-8"?><Passwords><password><![CDATA[${MASTER_PWD}]]></password><system_user_password><![CDATA[${MASTER_PWD}]]></system_user_password><sapadm_password><![CDATA[${MASTER_PWD}]]></sapadm_password><xs_master_password><![CDATA[${MASTER_PWD}]]></xs_master_password><org_manager_password><![CDATA[${MASTER_PWD}]]></org_manager_password><master_password><![CDATA[${MASTER_PWD}]]></master_password></Passwords>
EOF
		if [ $? -ne 0 ]; then
		echo
			echo "Failed to import delivery units."
			postInstallSummary 1 "XSC Installation"
			appendRemainTasksStatusToPostSummary
			exit 1
		fi

		postInstallSummary 0 "XSC Installation"

		collectGarbage
	fi
}

#
# Install XSA and apps
#
installXSA() {
	local status=0
	if [ "${COMPONENT}" == "all" ]; then
		echo "Installing HANA Extended Services (XSA)..."
		local log_begin=`wc -l $LOG_FILE | cut -d' ' -f1`
		"${IMAGE_DIR}/${DATA_UNITS_DIR}/${HDB_LCM_COMP_DIR}/hdblcm" -s ${SID} -H ${HOST_NAME} --action=update --components=xs --xs_components=xsac_cockpit,xsac_hrtt,xsac_monitoring,xsac_portal_serv,xsac_sap_web_ide,xsac_services,xsac_ui5_fesv3 --configfile="${IMAGE_DIR}/${DATA_UNITS_DIR}/${HDB_LCM_COMP_DIR}/configurations/auto_install.cfg" --component_medium="${IMAGE_DIR}" --read_password_from_stdin=xml -b <<-EOF
<?xml version="1.0" encoding="UTF-8"?><Passwords><password><![CDATA[${MASTER_PWD}]]></password><system_user_password><![CDATA[${MASTER_PWD}]]></system_user_password><sapadm_password><![CDATA[${MASTER_PWD}]]></sapadm_password><xs_master_password><![CDATA[${MASTER_PWD}]]></xs_master_password><org_manager_password><![CDATA[${MASTER_PWD}]]></org_manager_password><master_password><![CDATA[${MASTER_PWD}]]></master_password></Passwords>
EOF
		status=$?
		local log_end=`wc -l $LOG_FILE | cut -d' ' -f1`
		if [ $status -ne 0 ] || sed -n "${log_begin},${log_end}p" $LOG_FILE | grep -i "fail\|error\|cannot find\|No such file\|is invalid" >& /dev/null; then
			echo
			echo "Failed to install HANA Extended Services (XSA)."
			postInstallSummary 1 "XSA Installation"
			appendRemainTasksStatusToPostSummary
			exit 1
		else
			postInstallSummary 0 "XSA Installation"
                        HAS_XSA=1
		fi

		chmod 755 /usr/sap/${SID}/home/.profile
		sed -i "s|^if \[ -f \$HOME/\.bashrc.*|if \[ -f \$HOME/\.bashrc -a -z \"\$SAPSYSTEMNAME\" \]; then|" /usr/sap/${SID}/home/.profile

		# Update the xsa web dispatcher template
		# Overwrite the contents of the xsa web dispatch template with some settings for HXE.
		webtemplate="/hana/shared/${SID}/xs/controller_data/controller/router/webdispatcher/conf/sapwebdisp.template"
		if [ -f "$webtemplate" ]; then
			echo "mv ${webtemplate} ${webtemplate}.orig" | su -l ${HXE_ADM_USER}
		fi
		su -l ${HXE_ADM_USER} -c "cat > $webtemplate <<EOF
#This template has been customized for use with HANA, express edition
#to work in a resource constrained environment.

#Reduce the maximum number of simultaneuous connections from 2000 to 500
icm/max_conn = 500

#Reduce the maximum number of services from 500 to 100
icm/max_services = 100

#Reduce the maximum number of routes from 500 to 100
wdisp/max_systems = 100

#Reduce the maximum number of instances per application from 64 to 10
wdisp/max_servers = 10

#Increase the frequency of slot cleanup
wdisp/system_slot_cleanup_threshold = 0

EOF
"

		#Start the database. The hdblcm update action does not start the database.
		echo "HDB start" | su -l ${HXE_ADM_USER}

		# Copy create_tenantdb.sh to <hxeadm home>/bin directory
		echo "mkdir -p /usr/sap/${SID}/home/bin" | su -l ${HXE_ADM_USER}
		echo "cp -p ${IMAGE_DIR}/create_tenantdb.sh /usr/sap/${SID}/home/bin" | su -l ${HXE_ADM_USER}
		echo "chmod 755 /usr/sap/${SID}/home/bin/create_tenantdb.sh" | su -l ${HXE_ADM_USER}

		# Copy hxe_telemetry.sh to <hxeadm home>/bin directory
		echo "mkdir -p /usr/sap/${SID}/home/bin" | su -l ${HXE_ADM_USER}
		echo "cp -p ${IMAGE_DIR}/hxe_telemetry.sh /usr/sap/${SID}/home/bin" | su -l ${HXE_ADM_USER}
		echo "chmod 755 /usr/sap/${SID}/home/bin/hxe_telemetry.sh" | su -l ${HXE_ADM_USER}

		# Copy register_cockpit.sh to <hxeadm home>/bin directory
		echo "mkdir -p /usr/sap/${SID}/home/bin" | su -l ${HXE_ADM_USER}
		echo "cp -p ${IMAGE_DIR}/register_cockpit.sh /usr/sap/${SID}/home/bin" | su -l ${HXE_ADM_USER}
		echo "chmod 755 /usr/sap/${SID}/home/bin/register_cockpit.sh" | su -l ${HXE_ADM_USER}
	        
               # Update Versions
               updateVersionFile

		collectGarbage
        fi
}

#
#Update Versions
#
updateVersionFile() {
        local install_type=""
        if [ -f /usr/sap/${SID}/SYS/global/hdb/hxe_info.txt ]; then
                install_type=$(grep ^INSTALL_TYPE /usr/sap/${SID}/SYS/global/hdb/hxe_info.txt | cut -d'=' -f2)
                rm -f /usr/sap/${SID}/SYS/global/hdb/hxe_info.txt
        fi

        # Copy hxe_info.txt to /usr/sap/<SID>/SYS/global/hdb directory
        su -l ${HXE_ADM_USER} -c "cp -p ${IMAGE_DIR}/hxe_info.txt /usr/sap/${SID}/SYS/global/hdb/hxe_info.txt"
        su -l ${HXE_ADM_USER} -c "chmod 644 /usr/sap/${SID}/SYS/global/hdb/hxe_info.txt"

        if [ -n "${install_type}" ]; then
          sed -i "s/^INSTALL_TYPE*=.*/INSTALL_TYPE=$install_type/" /usr/sap/${SID}/SYS/global/hdb/hxe_info.txt
        fi

        local install_date=`date --utc`
        sed -i "s/^INSTALL_DATE.*=.*/INSTALL_DATE=$install_date/" /usr/sap/${SID}/SYS/global/hdb/hxe_info.txt

        if [ $HAS_XSA -ne 1 ]; then
                sed -i "/^XSA/d" /usr/sap/${SID}/SYS/global/hdb/hxe_info.txt
        fi
}

#
# Do garbage collection
#
collectGarbage() {
	su -l ${HXE_ADM_USER} -c "${IMAGE_DIR}/hxe_gc.sh<<-EOF
${MASTER_PWD}
EOF"
}

#
# Execute hxe_optimize.sh script
#
execOptimized() {
	local status=0

	echo "Optimizing HDB server..."
	if [ "${COMPONENT}" != "all" ]; then
		su -l ${HXE_ADM_USER} -c "${IMAGE_DIR}/hxe_optimize.sh <<-EOF
${INSTANCE}
${MASTER_PWD}
EOF"
	else
		local option=""
		if [ $DO_NOT_REGISTER_RESOURCE -eq 1 ]; then
			option=" --no_reg"
		fi
		if [ $SETUP_PROXY -eq 1 ]; then
			su -l ${HXE_ADM_USER} -c "${IMAGE_DIR}/hxe_optimize.sh ${option}<<-EOF
${INSTANCE}
${MASTER_PWD}
${MASTER_PWD}
${MASTER_PWD}
${MASTER_PWD}
${MASTER_PWD}
${MASTER_PWD}
Y
${PROXY_HOST}
${PROXY_PORT}
${NO_PROXY_HOST}
EOF"
		else
			su -l ${HXE_ADM_USER} -c "${IMAGE_DIR}/hxe_optimize.sh ${option}<<-EOF
${INSTANCE}
${MASTER_PWD}
${MASTER_PWD}
${MASTER_PWD}
${MASTER_PWD}
${MASTER_PWD}
${MASTER_PWD}
N
EOF"
		fi
	fi

	if [ $? -ne 0 ]; then
		status=1
	fi
	postInstallSummary $status "HXE Optimization"

	return $status
}

#
# Install SHINE
#
installShine() {
	local status=0
	if [ "${COMPONENT}" == "all" -a $INSTALL_SHINE -eq 1 ]; then
		echo "Install SHINE..."
		su -l ${HXE_ADM_USER} -c "${IMAGE_DIR}/install_shine.sh <<-EOF
${INSTANCE}
${MASTER_PWD}
${MASTER_PWD}
${MASTER_PWD}
${MASTER_PWD}
Y
EOF"
		if [ $? -ne 0 ]; then
			status=1
		fi
		postInstallSummary $status "Shine Installation"
	fi

	return $status
}

#
# Install EA Designer
#
installEADesigner() {
        local status=0
        if [ "${COMPONENT}" == "all" -a $INSTALL_EAD -eq 1 ]; then
                echo "Install SAP EA Designer..."
                su -l ${HXE_ADM_USER} -c "${IMAGE_DIR}/install_eadesigner.sh <<-EOF
${INSTANCE}
${MASTER_PWD}
${MASTER_PWD}
Y
EOF"
                if [ $? -ne 0 ]; then
                        status=1
                fi
                postInstallSummary $status "EA Designer Installation"
        fi

        return $status
}

#
# Install SA
#
installSA() {
	local status=0
	if [ $INSTALL_SA -eq 1 ]; then
		echo "Install SAP HANA streaming analytics..."

		# Create new tenant database for SA
		if ! hasDatabase $SA_TENANT_DB; then
			if [ "${COMPONENT}" == "all" ]; then
				if [ $SETUP_PROXY -eq 1 ]; then
					su -l ${HXE_ADM_USER} -c "/usr/sap/${SID}/home/bin/create_tenantdb.sh <<-EOF
${MASTER_PWD}
XSA_ADMIN
${MASTER_PWD}
TEL_ADMIN
${MASTER_PWD}
${INSTANCE}
${SA_TENANT_DB}
Y
${PROXY_HOST}
${PROXY_PORT}
${NO_PROXY_HOST}
EOF"
				else
					su -l ${HXE_ADM_USER} -c "/usr/sap/${SID}/home/bin/create_tenantdb.sh <<-EOF
${MASTER_PWD}
XSA_ADMIN
${MASTER_PWD}
TEL_ADMIN
${MASTER_PWD}
${INSTANCE}
${SA_TENANT_DB}
N
EOF"
				fi
				if [ $? -ne 0 ]; then
					postInstallSummary 1 "Streaming Analytics Installation"
					appendRemainTasksStatusToPostSummary
					return 1
				fi
			else
				echo "Create tenant database ${SA_TENANT_DB}..."
				execSQL ${INSTANCE} SystemDB SYSTEM ${MASTER_PWD} "CREATE DATABASE ${SA_TENANT_DB} SYSTEM USER PASSWORD \"${MASTER_PWD}\""
				if [ $? -ne 0 ]; then
					postInstallSummary 1 "Streaming Analytics Installation"
					appendRemainTasksStatusToPostSummary
					return 1
				fi
			fi
		fi

		# Install SA
		${IMAGE_DIR}/install_hsa.sh <<-EOF
${SID}
${INSTANCE}
${HOST_NAME}
${MASTER_PWD}
${MASTER_PWD}
${SA_TENANT_DB}
${MASTER_PWD}
Y
EOF
		if [ $? -ne 0 ]; then
			status=1
		fi
		postInstallSummary $status "Streaming Analytics Installation"
	fi

	return $status
}

#
# Install SDI
#
installSDI() {
	local status=0
	if [ $INSTALL_SDI -eq 1 ]; then
		echo "Install SAP HANA smart data integration..."

		# Create new tenant database for SDI
		if ! hasDatabase $SDI_TENANT_DB; then
			if [ "${COMPONENT}" == "all" ]; then
				if [ $SETUP_PROXY -eq 1 ]; then
					su -l ${HXE_ADM_USER} -c "/usr/sap/${SID}/home/bin/create_tenantdb.sh <<-EOF
${MASTER_PWD}
XSA_ADMIN
${MASTER_PWD}
TEL_ADMIN
${MASTER_PWD}
${INSTANCE}
${SDI_TENANT_DB}
Y
${PROXY_HOST}
${PROXY_PORT}
${NO_PROXY_HOST}
EOF"
				else
					su -l ${HXE_ADM_USER} -c "/usr/sap/${SID}/home/bin/create_tenantdb.sh <<-EOF
${MASTER_PWD}
XSA_ADMIN
${MASTER_PWD}
TEL_ADMIN
${MASTER_PWD}
${INSTANCE}
${SDI_TENANT_DB}
N
EOF"
				fi
				if [ $? -ne 0 ]; then
					postInstallSummary 1 "SDI Installation"
					appendRemainTasksStatusToPostSummary
					return 1
				fi
			else
				echo "Create tenant database ${SDI_TENANT_DB}..."
				execSQL ${INSTANCE} SystemDB SYSTEM ${MASTER_PWD} "CREATE DATABASE ${SDI_TENANT_DB} SYSTEM USER PASSWORD \"${MASTER_PWD}\""
				if [ $? -ne 0 ]; then
					postInstallSummary 1 "SDI Installation"
					appendRemainTasksStatusToPostSummary
					return 1
				fi
			fi
		fi

		# Install SDI
		su -l ${HXE_ADM_USER} -c "${IMAGE_DIR}/install_sdi.sh <<-EOF
${INSTANCE}
${HOST_NAME}
${MASTER_PWD}
${SDI_TENANT_DB}
${MASTER_PWD}
Y
EOF"
		if [ $? -ne 0 ]; then
			status=1
		fi
		postInstallSummary $status "SAP HANA smart data integration Installation"
	fi

	return $status
}

#
# Install DP Agent
#
installDPA() {
	local status=0
	if [ $INSTALL_DPA -eq 1 ]; then
		echo "Install DP Agent..."

		"${IMAGE_DIR}/${DATA_UNITS_DIR}/${DPA_DIR}/hdbinst" --agent_admin_port $DPA_ADMIN_PORT --agent_listener_port $DPA_LISTENER_PORT --path "$DPA_INSTALL_PATH" --user_id $DPA_USER <<-EOF


EOF
		if [ $? -ne 0 ]; then
			status=1
		fi
		postInstallSummary $status "DP Agent Installation"
	fi

	return $status
}

#
# Install SAP HANA External Machine Learning Library
#
installEML() {
	local status=0
	if [ $INSTALL_EML -eq 1 ]; then
		echo "Install SAP HANA External Machine Learning Library..."
		su -l ${HXE_ADM_USER} -c "${IMAGE_DIR}/install_eml.sh <<-EOF
Y
EOF"
		if [ $? -ne 0 ]; then
			status=1
		fi
		postInstallSummary $status "EML Installation"
	fi

	return $status
}

#
# Output the post-install summary of installations
#
postInstallSummary(){
	if [ $1 -ne 0 ]; then
		SUMMARY+="$2...(FAIL)\n"
	else
		SUMMARY+="$2...(OK)\n"
	fi
}

appendRemainTasksStatusToPostSummary()
{
		if [[ $SUMMARY != *"Server Installation"* ]] ; then
				SUMMARY+="Server Installation...(NOT STARTED)\n"
		fi

		if [[ "$SUMMARY" != *"XSC Installation"* ]]; then
			SUMMARY+="XSC Installation...(NOT STARTED)\n"
		fi

		if [[ "${COMPONENT}" == "all" ]]; then
				if [[ $SUMMARY != *"XSA Installation"* ]]; then
						SUMMARY+="XSA Installation...(NOT STARTED)\n"
				fi

				if [[ $SUMMARY != *"HXE Optimization"* ]]; then
						SUMMARY+="HXE Optimization...(NOT STARTED)\n"
				fi

				if [[ ( $SUMMARY != *"Shine Installation"* ) && ( $INSTALL_SHINE -eq 1 ) ]]; then
						SUMMARY+="Shine Installation...(NOT STARTED)\n"
				fi

				if [[ ( $SUMMARY != *"EA Designer Installation"* ) && ( $INSTALL_EAD -eq 1 ) ]]; then
						SUMMARY+="EA Designer Installation...(NOT STARTED)\n"
				fi
		fi

		if [[ ( $SUMMARY != *"Streaming Analytics Installation"* ) && ( $INSTALL_SA -eq 1 ) ]]; then
				SUMMARY+="Streaming Analytics Installation...(NOT STARTED)\n"
		fi

		if [[ ( $SUMMARY != *"SAP HANA smart data integration Installation"* ) && ( $INSTALL_SDI -eq 1 ) ]]; then
			SUMMARY+="SAP HANA smart data integration Installation...(NOT STARTED)\n"
		fi

		if [[ ( $SUMMARY != *"DP Agent"* ) && ( $INSTALL_DPA -eq 1 ) ]]; then
			SUMMARY+="DP Agent (NOT STARTED)\n"
		fi

		if [[ ( $SUMMARY != *"EML Installation"* ) && ( $INSTALL_EML -eq 1 ) ]]; then
				SUMMARY+="EML Installation...(NOT STARTED)\n"
		fi

		echo -e "$SUMMARY"
		echo
		echo "For more details, refer to $LOG_FILE"
		echo

		if [[ "$SUMMARY" =~ FAIL ]]; then
			return 1
		else
			return 0
		fi

}

isOSPackageInstalled() {
	if [[ ${OS_VENDOR} =~ [Ss][Ll][Ee][Ss] ]] || [[ ${OS_VENDOR} =~ [Rr][Ee][Dd][Hh][Aa][Tt] ]];then
		rpm -q  ${1} >& /dev/null
		return $?
	fi
	if [[ ${OS_VENDOR} =~ [Uu][Bb][Uu][Nn][Tt][Uu] ]];then
		dpkg -s ${1} >& /dev/null
		return $?
	fi
}

#
# Trim leading and trailing spaces
#
trim()
{
	trimmed="$1"
	trimmed=${trimmed%% }
	trimmed=${trimmed## }
	echo "$trimmed"
}

# Get value from INI file
# $1 - init file
# $2 - [section] name
# $3 - property name
getINIFileValue() {
	sed -nr "/^\[$2\]/ { :l /^$3[ ]*=/ { s/.*=[ ]*//; p; q;}; n; b l;}" $1
}

#
# Compare version
#
# Print:
#  -1 if $1 < $2
#   0 if $1 = $2
#   1 if $1 > $2
compareVersion() {
	local older_version=`echo -e "$1\n$2" | sort -V | head -n1`
	if [ "$1" == "$2" ]; then
		echo 0
	elif [  "$1" == "$older_version" ];then
		echo -1
	else
		echo 1
	fi
}

# Clean temporary files on exit
cleanup() {
	local rv=$?

	if [ -d ${OPENSSL_TMP} ]; then
		rm -rf ${OPENSSL_TMP}
		ldconfig
	fi

	if [ -f ${OPENSSL_LD_CONF} ]; then
		rm -f ${OPENSSL_LD_CONF}
	fi

	if [ $CLEANUP_CHKCONFIG -eq 1 ]; then
		rm -f /usr/sbin/chkconfig
	fi

	exit $rv
}

#########################################################
# Main
#########################################################
PLATFORM=""
os=`uname -o | tr '[:upper:]' '[:lower:]'`
machine=`uname -m | tr '[:upper:]' '[:lower:]'`
if [[ "$os" =~ linux ]]; then
	if [ "$machine" == "x86_64" ] || [ "$machine" == "amd64" ] || [ "$machine" == "i386" ] || [ "$machine" == "i686" ]; then
		PLATFORM="LINUX_X86_64"
		PLATFORM_XSA_RT="LINUX_X86_64"
		PLATFORM_DPA="LIN_X86_64"
	elif [ "$machine" == "ppc64le" ]; then
		PLATFORM="LINUX_PPC64LE"
		PLATFORM_XSA_RT="LINUX_PPC64LE"
	fi
fi
OS_VENDOR=""
if [[ -f /etc/SuSE-release ]]; then
	OS_VENDOR="SLES"
elif [[ -f /etc/redhat-release ]]; then
	OS_VENDOR="RedHat"
elif [[ -f /etc/os-release ]]; then
	OS_VENDOR=`grep "^NAME=" /etc/os-release | cut -d'"' -f2`
fi
SKIP_OS_CHECK=0
HAS_OPENSSL_V2=0
OPENSSL_VERSION=
OPENSSL_URL=
OPENSSL_TMP=/tmp/openssl.$$
OPENSSL_LD_CONF=/etc/ld.so.conf.d/ssl2-$$.conf
REQUIRE_OS_PACKAGE="libpam-cracklib gawk make gcc libltdl7 libaio1 zip unzip"
MISSING_OS_PACKAGE=""
CLEANUP_CHKCONFIG=0
HXE_DIR="HANA_EXPRESS_20"
DATA_UNITS_DIR="DATA_UNITS"
HDB_SERVER_COMP_DIR="HDB_SERVER_${PLATFORM}"
HDB_LCM_COMP_DIR="HDB_LCM_${PLATFORM}"
XSA_RT_COMP_DIR="XSA_RT_10_${PLATFORM_XSA_RT}"
XSA_CONTENT_COMP_DIR="XSA_CONTENT_10"
HANA_COCKPIT_COMP_DIR="HANA_COCKPIT_20"
WEB_IDE_COMP_DIR="XSAC_SAP_WEB_IDE_20"
SA_DIR="HANA_STREAMING_4H20_02_HXE"
SDI_DIR="SAP_HANA_SDI_20"
DPA_DIR="HANA_DP_AGENT_20_${PLATFORM_DPA}"
EML_DIR="HDB_EML_AFL_10_${PLATFORM}"
WEB_IDE_ZIP_FILE="XSACSAPWEBIDE02_1.zip"
WEB_IDE_EXT_FILE="sap-xsac-devx-4.2.18-hxe.mtaext"
HANA_COCKPIT_ZIP_FILE="XSACCOCKPIT03_9.zip"
BUILD_VERSION="@{BUILD_VERSION_DATE}"
HAS_SERVER=0
HAS_XSA=0
ADD_XSA=0
SETUP_PROXY=0
SYSTEM_PROXY_HOST=""
SYSTEM_PROXY_PORT=""
SYSTEM_NO_PROXY_HOST=""
PROXY_HOST=""
PROXY_PORT=""
NO_PROXY_HOST=""

PROG_DIR="$(cd "$(dirname ${0})"; pwd)"
PROG_NAME=`basename $0`

IMAGE_DIR="${PROG_DIR}/${HXE_DIR}"
IMAGE_HAS_SERVER=0
IMAGE_HAS_XSA=0
IMAGE_HAS_SA=0
IMAGE_HAS_SDI=0
IMAGE_HAS_DPA=0
IMAGE_HAS_SHINE=0
IMAGE_HAS_EAD=0
IMAGE_HAS_EML=0

COMPONENT="server"
HOST_NAME=`hostname`
SID="HXE"
HXE_ADM_USER="hxeadm"
INSTANCE="90"

MASTER_PWD=""

HDBLCM_ARGS=()

INSTALL_SHINE=0
INSTALL_EAD=0
INSTALL_SA=0
INSTALL_SDI=0
INSTALL_DPA=0
INSTALL_EML=0
SA_TENANT_DB=""
SDI_TENANT_DB=""

DPA_INSTALL_PATH="/usr/sap/dataprovagent"
DPA_USER=""
DPA_LISTENER_PORT=""
DPA_ADMIN_PORT=""

BATCH_MODE=0
DO_NOT_REGISTER_RESOURCE=0

SUMMARY="#########################################################################\n# Summary after execution \t\t\t\t\t\t#\n#########################################################################\n"

DATE=$(date +"%Y-%m-%d_%H.%M.%S")
LOG_FILE="/var/tmp/setup_hxe_${DATE}.log"

if [ "$PLATFORM" != "LINUX_X86_64" ] && [ -d "${IMAGE_DIR}/${DATA_UNITS_DIR}/HDB_LCM_LINUX_X86_64" ]; then
	echo "Invalid platform.  This is HANA, express edition on Linux X86_64"
	exit 1;
elif [ "$PLATFORM" != "LINUX_PPC64LE" ] && [ -d "${IMAGE_DIR}/${DATA_UNITS_DIR}/HDB_LCM_LINUX_PPC64LE" ]; then
	echo "Invalid platform.  This is HANA, express edition on Linux PowerPC 64-bit (little endian)."
	exit 1;
fi

#
# Parse argument
#
if [ $# -gt 0 ]; then
	PARSED_OPTIONS=`getopt -n "$PROG_NAME" -a -o bi: --long hdblcm_cfg:,hostname:,sa_db:,sid:,no_reg,dpa_install_path:,dpa_user:,dpa_listener_port:,dpa_admin_port:,skip_os_check -- "$@"`
	if [ $? -ne 0 -o "$#" -eq 0 ]; then
		exit 1
	fi

	# Process command line arguments
	eval set -- "$PARSED_OPTIONS"
	while true
	do
		case "$1" in
		-b)
			BATCH_MODE=1
			shift;;
		-hdblcm_cfg|--hdblcm_cfg)
			IFS=,
			array=($2)
			for i in "${!array[@]}"
			do
				param=`trim "${array[$i]}"`
				HDBLCM_ARGS[$i]="--${param}"
			done
			unset IFS
			shift 2;;
		-hostname|--hostname)
			HOST_NAME="$2"
			if ! $(isValidHostName "$HOST_NAME"); then
				echo
				echo "\"$HOST_NAME\" is not a valid host name or IP address."
				echo
			fi
			shift 2;;
		-i)
			if ! [[ ${2} =~ ^[0-9]+$ ]] ; then
				echo
				echo "\"$2\" is not a number.  Enter instance number between 00 and 99."
				echo
				exit 1
			elif [ ${2} -ge 0 -a ${2} -le 99 ]; then
				if [[ ${2} =~ ^[0-9]$ ]] ; then
					INSTANCE="0${2}"
				else
					INSTANCE="${2}"
				fi
			else
				echo
				echo "Invalid number.  Enter instance number between 00 and 99."
				echo
				exit 1
			fi
			shift 2;;
		-sa_db|--sa_db)
			SA_TENANT_DB="$2"
			shift 2;;
		-sdi_db|--sdi_db)
			SDI_TENANT_DB="$2"
			shift 2;;
		-sid|--sid)
			SID="$2"
			if [[ ${SID} =~ ^[A-Z][A-Z,0-9][A-Z,0-9]$ ]]; then
				HXE_ADM_USER=`echo ${SID}adm | tr '[:upper:]' '[:lower:]'`
			else
				echo
				echo "Invalid SAP HANA system ID.  This must be a three characters string."
				echo "First character has to be an upper case letter."
				echo "Second and third characters can be upper case letter or a decimal digit."
				echo
				exit 1
			fi
			shift 2;;
		-skip_os_check|--skip_os_check)
			SKIP_OS_CHECK=1
			shift;;
		-no_reg|--no_reg)
			DO_NOT_REGISTER_RESOURCE=1
			shift 1;;
		-dpa_install_path|--dpa_install_path)
			DPA_INSTALL_PATH="$2"
			shift 2;;
		-dpa_user|--dpa_user)
			DPA_USER="$2"
			shift 2;;
		-dpa_listener_port|--dpa_listener_port)
			DPA_LISTENER_PORT="$2"
			shift 2;;
		-dpa_admin_port|--dpa_admin_port)
			DPA_ADMIN_PORT="$2"
			shift 2;;
		--)
			shift
			break;;
		*)
			echo "Invalid \"$1\" argument."
			exit 1
		esac
	done
fi

checkRootUser

checkBash

if [ $SKIP_OS_CHECK -ne 1 ]; then
	if [[ ${OS_VENDOR} =~ [Uu][Bb][Uu][Nn][Tt][Uu] ]];then
		checkOSPackage
		checkOpenSSL
	fi
fi

# Create log file
if [ -f $LOG_FILE ]; then
	rm -f $LOG_FILE
fi
touch $LOG_FILE
chmod 640 $LOG_FILE
date +"%Y-%m-%d %H.%M.%S :" >> $LOG_FILE
echo "" >> $LOG_FILE

if [ $BATCH_MODE -eq 0 ]; then
	if [ $SKIP_OS_CHECK -ne 1 ]; then
		if [[ ${OS_VENDOR} =~ [Uu][Bb][Uu][Nn][Tt][Uu] ]];then
			promptOSPackage
			installOSPackage # Need to install libpam-cracklib for password check
			if [ ! -e /usr/sbin/chkconfig ]; then
				# Temporary workaround "chkconfig --add"
				cat > /usr/sbin/chkconfig <<-EOF
#!/bin/bash

if [ "\$1" == "-a" ] || [ "\$1" == "--add" ]; then
	echo "Add service \$2..."
	update-rc.d \$2 defaults
elif [ "\$1" == "-d" ] || [ "\$1" == "--del" ]; then
	echo "Delete service \$2..."
	update-rc.d \$2 remove
fi
EOF
				chmod 755 /usr/sbin/chkconfig
				CLEANUP_CHKCONFIG=1
			fi
			promptOpenSSL
		fi
	fi

	promptImageRootDir
	promptSID
	promptInstance
	searchSID
	if [ ${HAS_SERVER} -eq 1 -a ${HAS_XSA} -eq 0 -a ${IMAGE_HAS_XSA} -eq 1 ];then
		promptAddXSA
		checkCompatibleXSAVersion
	fi
	if [ ${ADD_XSA} -eq 0 ]; then
		if [ ${IMAGE_HAS_SERVER} -eq 0 ];then
			echo "Cannot install HANA, express edition."
			echo "Missing server component ${IMAGE_DIR}/${DATA_UNITS_DIR}/${HDB_SERVER_COMP_DIR}."
			echo "You need to download and extract \"Server only installer\" package (hxe.tgz) to"
			echo "${PROG_DIR}."
			exit 1
		else
			promptComponent
		fi
	fi

	promptHostName
	promptNewPwd "HDB master" "MASTER_PWD"

	if [ "${COMPONENT}" == "all" ]; then
		promptProxyInfo
	fi

	if [ "${COMPONENT}" == "all" ]; then
		if [ ${IMAGE_HAS_SHINE} -eq 1 ]; then
			promptInstallShine
		fi

		if [ ${IMAGE_HAS_EAD} -eq 1 ]; then
			promptInstallEADesigner
		fi
	fi

	if [ ${IMAGE_HAS_SA} -eq 1 ];then
		promptInstallSA
	fi

	if [ ${IMAGE_HAS_SDI} -eq 1 ];then
		promptInstallSDI
	fi

	if [ ${IMAGE_HAS_DPA} -eq 1 ];then
		promptInstallDPA
	fi

	if [ ${IMAGE_HAS_EML} -eq 1 ];then
		promptInstallEML
	fi
else # batch mode
	if [ $SKIP_OS_CHECK -ne 1 ]; then
		if [[ ${OS_VENDOR} =~ [Uu][Bb][Uu][Nn][Tt][Uu] ]];then
			installOSPackage # Need to install libpam-cracklib for password check
			if [ ! -e /usr/sbin/chkconfig ]; then
				# Temporary workaround "chkconfig --add"
				cat > /usr/sbin/chkconfig <<-EOF
#!/bin/bash

if [ "\$1" == "-a" ] || [ "\$1" == "--add" ]; then
	echo "Add service \$2..."
	update-rc.d \$2 defaults
elif [ "\$1" == "-d" ] || [ "\$1" == "--del" ]; then
	echo "Delete service \$2..."
	update-rc.d \$2 remove
fi
EOF
				chmod 755 /usr/sbin/chkconfig
				CLEANUP_CHKCONFIG=1
			fi
		fi
	fi

	read -r -s MASTER_PWD
	if ! isValidPassword $MASTER_PWD; then
		exit 1
	fi

	if [ ! -f "${IMAGE_DIR}/${DATA_UNITS_DIR}/${HDB_LCM_COMP_DIR}/hdblcm" ]; then
		echo
		echo "\"${IMAGE_DIR}\" does not contain HANA, express edition installer."
		echo
		exit 1
	fi

	if [ -d "${IMAGE_DIR}/${DATA_UNITS_DIR}/${XSA_RT_COMP_DIR}" ]; then
		IMAGE_HAS_XSA=1
		COMPONENT="all"

		if [ ! -f "${IMAGE_DIR}/${DATA_UNITS_DIR}/${WEB_IDE_COMP_DIR}/${WEB_IDE_ZIP_FILE}" ]; then
			echo
			echo "Invalid HANA, express edition installer directory."
			echo "Web IDE file \"${IMAGE_DIR}/${DATA_UNITS_DIR}/${WEB_IDE_COMP_DIR}/${WEB_IDE_ZIP_FILE}\" does not exist."
			echo
			exit 1
		elif [ ! -f "${IMAGE_DIR}/${DATA_UNITS_DIR}/${HANA_COCKPIT_COMP_DIR}/${HANA_COCKPIT_ZIP_FILE}" ]; then
			echo
			echo "Invalid HANA, express edition installer directory."
			echo "Cockpit file \"${IMAGE_DIR}/${DATA_UNITS_DIR}/${HANA_COCKPIT_COMP_DIR}/${HANA_COCKPIT_ZIP_FILE}\" does not exist."
			echo
			exit 1
		fi

		# Proxy
		getSystemHTTPProxy
		if [ -n "$SYSTEM_PROXY_HOST" ] && [ -n "$SYSTEM_PROXY_PORT" ]; then
			SETUP_PROXY=1
			PROXY_HOST="$SYSTEM_PROXY_HOST"
			PROXY_PORT="$SYSTEM_PROXY_PORT"
			NO_PROXY_HOST="$SYSTEM_NO_PROXY_HOST"
		fi

		if [ -f "${IMAGE_DIR}/install_shine.sh" ]; then
			IMAGE_HAS_SHINE=1
			INSTALL_SHINE=1
		fi
		if [ -f "${IMAGE_DIR}/install_eadesigner.sh" ]; then
			IMAGE_HAS_EAD=1
			INSTALL_EAD=1
		fi
	fi

	if [ -f "${IMAGE_DIR}/install_hsa.sh" ]; then
		IMAGE_HAS_SA=1
		INSTALL_SA=1
	fi

	if [ -f "${IMAGE_DIR}/install_sdi.sh" ]; then
		IMAGE_HAS_SDI=1
		INSTALL_SDI=1
	fi

	if [ -d "${IMAGE_DIR}/${DATA_UNITS_DIR}/${DPA_DIR}" ]; then
		IMAGE_HAS_DPA=1
		INSTALL_DPA=1
	fi

	if [ -f "${IMAGE_DIR}/install_eml.sh" ]; then
		IMAGE_HAS_EML=1
		INSTALL_EML=1
	fi
fi

printSummary >& >(tee -a "$LOG_FILE")

# Capture setup output to log file
exec 0>&-
exec >& >( awk -v lfile="$LOG_FILE" '{ print $0; print strftime("%Y-%m-%d %H:%M:%S :"),$0 >> (lfile); fflush() }' )

# Call 'cleanup' if Control-C or terminated
trap 'cleanup' SIGINT SIGTERM EXIT

if [ $SKIP_OS_CHECK -ne 1 ]; then
	if [[ ${OS_VENDOR} =~ [Uu][Bb][Uu][Nn][Tt][Uu] ]];then
		downloadInstallOpenSSL
	fi
fi

installServer
installXSC
installXSA

execOptimized

installShine

installEADesigner

installSA

installSDI

installDPA

installEML

echo
echo

appendRemainTasksStatusToPostSummary
