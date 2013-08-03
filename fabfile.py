from fabric.api import env, cd, run


env.hosts = ['wilfred.webfactional.com']
env.directory = "~/src/ButlerBot"


def deploy():
    with cd(env.directory):
        run('git pull origin master')
        run('cabal install --user')
