vol_qry <- function(id, from, to) {
  graphql_query <- paste0(
    '{
      trafficData(trafficRegistrationPointId: "', id, '") {
        volume {
          byHour(from: "', from, '", to: "', to, '") {
            edges {
              node {
                from
                to
                total {
                  volumeNumbers {
                    volume
                  }
                }
              }
            }
          }
        }
      }
    }'
  )
  
}
