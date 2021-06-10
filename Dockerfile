FROM registry.gitlab.com/coverified/infrastructure/scala-base:latest

MAINTAINER Coverified <info@coverified.info>

# List arguments
ARG PROJECT_NAME
ARG PROJECT_VERSION
ARG MAIN_CLASS
ARG VOLUME_MNT_POINT

# Copy build arguments to environment variables
ENV ENV_PROJECT_NAME=$PROJECT_NAME
ENV ENV_PROJECT_VERSION=$PROJECT_VERSION
ENV ENV_MAIN_CLASS=$MAIN_CLASS

# Copy jar to container
COPY build/libs/$PROJECT_NAME-$PROJECT_VERSION-all.jar $WORKDIR

# Add a volume
VOLUME $VOLUME_MNT_POINT

# Actually executing the extractor
CMD java -cp $WORKDIR/$ENV_PROJECT_NAME-$ENV_PROJECT_VERSION-all.jar $ENV_MAIN_CLASS
