#lang pollen

◊h1{Deploy Haskell applications to Beanstalk}
◊h2{via CircleCI workflows}
◊m-article{
  ◊p{For a while now, I was accustomed to deploying Haskell applications by building them in a CI like CircleCI or TravisCI, then copying the resulting binaries compressed (via ◊a[#:href "https://upx.github.io/"]{UPX}) to S3, which were then copied over to the target servers via Ansible - a very nice and simple strategy we employed back when I worked at Zalora.}
  ◊p{At Anapi, I have some applications that we were deploying as microservices, and I didn't really want to burn up time with deployments, so we decided to run these apps on AWS Beanstalk, which allowed for some nice features like autoscaling and relieving me of tasks like managing the load balancing etc. The workflow I configured in CircleCI is pretty simple:}
  ◊p{◊ol{
    ◊li{Run the usual build &amp; test on push to Github}
    ◊li{If the build succeeds, then check if we are on a deployment branch, such as ◊code{staging}}
    ◊li{Compress binary and copy to S3 (append a hash and save it via CircleCI's workspaces}
    ◊li{On a deployment workflow step, copy binary from S3, and build app via Docker, push to AWS ECR}
    ◊li{Reload the Beanstalk environment}
    Here's an snippet from my ◊code{.circleci/config.yml}:
  }}
  ◊m-code-yaml{
version: 2
jobs:
  build:
    # ... build and test instructions here
    - run: stack build
    - run: stack test
    - run: mkdir -p workspace
    - run: echo "foo-$CIRCLE_SHA1" > workspace/s3-out
    - persist_to_workspace:
        root: workspace
        paths:
        - s3-out
    - run: |
        if [[ `git rev-parse --abbrev-ref HEAD` == "staging" ]]; then
          stack install --local-bin-path .
          /root/tools/upx foo
          aws s3 cp foo s3://foo-bucket/builds/foo-$CIRCLE_SHA1
        fi
    # ... rest of build steps
  deploy_staging:
    # ...
    docker:
      - image: kenny/foo:latest
    steps:
      - checkout
      - run: stack setup
      - setup_remote_docker
      - run: eval $(aws ecr get-login --region ap-southeast-1 --no-include-email)
      - attach_workspace:
          at: /tmp/workspace
      - run: aws s3 cp s3://foo-bucket/builds/$(cat /tmp/workspace/s3-out) foo
      - run: docker build --rm=false -t foo.dkr.ecr.ap-southeast-1.amazonaws.com/kenny/foo:$CIRCLE_SHA1 -f Dockerfile .
      - run: docker push foo.dkr.ecr.ap-southeast-1.amazonaws.com/kenny/foo:$CIRCLE_SHA1
      - run: aws/staging.sh $CIRCLE_SHA1
workflows:
  version: 2
  pipeline:
    jobs:
      - build
      - deploy_staging:
          requires:
            - build
          filters:
            branches:
              only:
                - staging
}
  ◊p{For the Docker images I tend to start with a very minimal distro and maintain seperate images for CI and Beanstalk. With proper caching strategy, our build times vary between 3-4 mins overall on the build and testing phase, and 1-2 minutes on the deploy step, which is fairly decent.}
  ◊p{One thing that bugs me is the CI running into OOM issues whenever a full rebuild is triggered (e.g. bumping Stackage releases), and the workaround is to run it with ◊code{stack build -j1}, but it also takes around 40 minutes to complete.}
}
◊m-back
