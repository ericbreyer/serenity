# bin/zsh
# Description: Run a command until it fails

# Usage: runTilFailure.sh <command>
# Example: runTilFailure.sh "echo 'Hello, World!'"

count=0

while true; do
    $1 > /dev/null 2>&1
    if [ $? -ne 0 ]; then
        break
    fi
    count=$((count+1))
    echo "Command succeeded $count times"
done

echo "Command failed after $count iterations"