FROM registry.gitlab.com/coverified/infrastructure/scala-base:latest

MAINTAINER Coverified <info@coverified.info>

# List arguments
ARG PROJECT_NAME
ARG PROJECT_VERSION
ARG MAIN_CLASS

# Copy jar to container
COPY build/libs/$PROJECT_NAME-$PROJECT_VERSION-all.jar $WORKDIR

# Set the environment variables
#ENV EXTRACTOR_API_URL=""
#ENV EXTRACTOR_PAGE_PROFILE_PATH="" TODO: Also add profile configs in her

# Actually executing the extractor
CMD "java -cp $WORKDIR/$PROJECT_NAME-$PROJECT_VERSION-all.jar $MAIN_CLASS"