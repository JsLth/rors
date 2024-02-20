yml <- yaml::read_yaml("https://raw.githubusercontent.com/GIScience/openrouteservice/master/docker-compose.yml")
yaml::write_yaml(yml, "inst/setup/docker-compose.yml")

yml <- yaml::read_yaml("https://raw.githubusercontent.com/GIScience/openrouteservice/main/ors-api/src/test/resources/application-test.yml")
yaml::write_yaml(yml, "inst/setup/ors-config.yml")

rm(yml)
