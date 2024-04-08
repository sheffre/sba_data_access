mkdir C:\dasba_export

cd \docker
docker build -t dasba .

docker container create -p 3838:3838 -v C:\dasba_export : /srv/shiny-server/dasba/dasba-export dasba --name dasba 

