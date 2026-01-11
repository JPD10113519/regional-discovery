### Region Selection Options:
###  * marks already done for code v1

## From Colin Woodard's American Nations:
# *AN_DeepSouth AN_ElNorte *AN_FarWest AN_GreaterAppalacia AN_LeftCoast AN_Midlands 
# *AN_NewNetherlands AN_SpanishCaribbean *AN_Tidewater AN_Yankeedom AN_NewFrance
## From MSU's American Communities Project:
# ACP_AfAmSouth *ACP_AgingFarmlands *ACP_BigCities ACP_CollegeTowns *ACP_EvangelicalHubs
# ACP_HispanicCenters *ACP_LDSEnclaves *ACP_MiddleSuburbs ACP_MilitaryPosts ACP_NativeAmericanLands
# ACP_Exurbs *ACP_GrayingAmerica *ACP_RuralMiddleAmerica ACP_UrbanBurbs ACP_WorkingClassCountry

SEED_NAME="ACP_BigCities"
VERSION_TAG="v1"
MAKE_INTERACTIVES=TRUE

## DO NOT CHANGE
JOB_NAME="${SEED_NAME}_${VERSION_TAG}"
mkdir -p output/${JOB_NAME}
mkdir -p output/${JOB_NAME}/tmp
mkdir -p output/${JOB_NAME}/logs

JOB_ID=$(sbatch --parsable \
  --job-name=${JOB_NAME} \
  --output=output/${JOB_NAME}/logs/pd_%A_%a.out \
  --error=output/${JOB_NAME}/logs/pd_%A_%a.err \
  --export=ALL,JOB_NAME=${JOB_NAME},SEED_NAME=${SEED_NAME} \
  code/init_batch.sh)
  
## processing as dependent job
sbatch --dependency=afterok:${JOB_ID} \
  --job-name=${JOB_NAME}_post \
  --output=output/${JOB_NAME}/logs/${JOB_NAME}_stitch.out \
  --error=output/${JOB_NAME}/logs/${JOB_NAME}_stitch.err \
  code/process.sh $JOB_NAME $SEED_NAME $MAKE_INTERACTIVES
