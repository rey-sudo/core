export function randAvatar() {
  const number = Math.floor(Math.random() * 49);
  return `https://space.auditocean.com/user-avatar/${number}-1.svg`;
}
