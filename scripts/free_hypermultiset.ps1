<#
.SYNOPSIS
    Runs the free-hypermultiset recurrence without a native AtomSpace build.

.DESCRIPTION
    This is a small Windows-side emulator/reference runner. It uses .NET
    BigInteger so that its results do not depend on machine integer width.
#>

[CmdletBinding()]
param(
    [ValidateRange(0, 1000)]
    [int] $MaxMembranes = 12,

    [switch] $ValidatePublished
)

Add-Type -AssemblyName System.Numerics

$treeCount = [System.Numerics.BigInteger[]]::new($MaxMembranes + 2)
$divisorSums = [System.Numerics.BigInteger[]]::new($MaxMembranes + 2)
$treeCount[1] = 1

for ($multiple = 1; $multiple -le $MaxMembranes + 1; $multiple++) {
    $divisorSums[$multiple] = 1
}

for ($nodes = 2; $nodes -le $MaxMembranes + 1; $nodes++) {
    $sum = [System.Numerics.BigInteger]::Zero
    for ($part = 1; $part -lt $nodes; $part++) {
        $sum += $treeCount[$nodes - $part] * $divisorSums[$part]
    }
    $treeCount[$nodes] = $sum / ($nodes - 1)

    for ($multiple = $nodes; $multiple -le $MaxMembranes + 1;) {
        $divisorSums[$multiple] += $nodes * $treeCount[$nodes]
        if ($multiple -gt ($MaxMembranes + 1) - $nodes) {
            break
        }
        $multiple += $nodes
    }
}

$counts = for ($membranes = 0; $membranes -le $MaxMembranes; $membranes++) {
    [pscustomobject]@{
        Membranes = $membranes
        Count = $treeCount[$membranes + 1].ToString()
    }
}

if ($ValidatePublished) {
    $published = @(
        '1', '1', '2', '4', '9', '20', '48',
        '115', '286', '719', '1842', '4766', '12486'
    )
    if ($MaxMembranes -lt $published.Count - 1) {
        throw "Validation requires MaxMembranes >= $($published.Count - 1)."
    }
    for ($index = 0; $index -lt $published.Count; $index++) {
        if ($counts[$index].Count -ne $published[$index]) {
            throw "Published sequence mismatch at M($index)."
        }
    }
    Write-Output "Published sequence validation passed."
}

$counts
