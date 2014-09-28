# rsv

Webserver that tracks changes on websites and publish changes as RSS. Data is captured in table (CSV-like) form, hence the name RSV.
To know what makes a new item, the algorithm looks at the similarity of different parts in the html page, as well as the similarity between different versions of the same html page.

## Prerequisites

Postgres installation
https://www.digitalocean.com/community/tutorials/how-to-install-and-use-postgresql-on-ubuntu-14-04
http://stackoverflow.com/questions/17633422/psql-fatal-database-user-does-not-exist
For ubuntu 11:
http://stackoverflow.com/questions/19774742/pgadmin3-server-connection-and-authentication-error

## Running

To start a web server for the application, run:

    lein ring server-headless

## License

Copyright © 2014 FIXME
