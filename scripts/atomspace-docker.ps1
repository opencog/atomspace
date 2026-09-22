[CmdletBinding()]
param(
    [ValidateSet('build', 'shell', 'test', 'ci')]
    [string] $Action = 'test',

    [string] $Image = 'atomspace-dev',

    [int] $Jobs = 2
)

$ErrorActionPreference = 'Stop'
$repo = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path
$dockerfile = Join-Path $repo 'docker\atomspace\Dockerfile'

if (-not (Get-Command docker -ErrorAction SilentlyContinue)) {
    throw 'Docker is required. Install and start Docker Desktop, then retry.'
}

if ($Action -eq 'build') {
    & docker build --tag $Image --file $dockerfile $repo
    exit $LASTEXITCODE
}

& docker image inspect $Image *> $null
if ($LASTEXITCODE -ne 0) {
    & docker build --tag $Image --file $dockerfile $repo
    if ($LASTEXITCODE -ne 0) {
        exit $LASTEXITCODE
    }
}

$mount = '{0}:/workspace' -f $repo
$command = if ($Action -eq 'shell') {
    @('bash')
} elseif ($Action -eq 'ci') {
    @(
        'bash', '-lc',
        @'
set -o pipefail
log=/workspace/build/tests/Testing/Temporary/LastTest.log
print_log() {
    if [ -f "$log" ]; then
        echo "===== AtomSpace test log ====="
        cat "$log"
    fi
}
trap print_log EXIT
cmake -S /workspace -B /workspace/build -DCMAKE_BUILD_TYPE=Release
cmake --build /workspace/build --parallel $JOBS
cmake --build /workspace/build --target tests --parallel $JOBS
cmake --build /workspace/build --target check --parallel $JOBS
cmake --build /workspace/build --target install --parallel $JOBS
cmake --build /workspace/build --target examples --parallel $JOBS
'@.Replace('$JOBS', $Jobs)
    )
} else {
    @(
        'bash', '-lc',
        'cmake -S /workspace -B /workspace/build -DCMAKE_BUILD_TYPE=Release && ' +
        'cmake --build /workspace/build --parallel && ' +
        'cmake --build /workspace/build --target check --parallel'
    )
}

& docker run --rm --interactive --tty --volume $mount $Image @command
exit $LASTEXITCODE
